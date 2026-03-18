use std::pin::Pin;
use std::time::{Duration, SystemTime};

use tokio_stream::Stream;
use tokio_stream::wrappers::ReceiverStream;

use crate::lisp::Config;

use crate::proto::common::v1alpha8::metrics::Metric;
use crate::proto::microgrid::v1alpha18::{
    AckElectricalComponentErrorRequest, AugmentElectricalComponentBoundsRequest,
    AugmentElectricalComponentBoundsResponse, GetMicrogridResponse,
    ListElectricalComponentConnectionsRequest, ListElectricalComponentConnectionsResponse,
    ListElectricalComponentsRequest, ListElectricalComponentsResponse, ListSensorRequest,
    ListSensorsResponse, PowerType, PutElectricalComponentInStandbyRequest,
    ReceiveElectricalComponentTelemetryStreamRequest,
    ReceiveElectricalComponentTelemetryStreamResponse, ReceiveSensorTelemetryStreamRequest,
    ReceiveSensorTelemetryStreamResponse, SetElectricalComponentPowerRequest,
    SetElectricalComponentPowerRequestStatus, SetElectricalComponentPowerResponse,
    StartElectricalComponentRequest, StopElectricalComponentRequest, microgrid_server,
};

pub struct MicrogridServer {
    pub config: Config,
    pub timeout_tracker: crate::timeout_tracker::TimeoutTracker,
}

impl MicrogridServer {
    pub fn new(config: Config) -> Self {
        let timeout_tracker = crate::timeout_tracker::TimeoutTracker::new();

        let new = Self {
            config,
            timeout_tracker,
        };

        new.start_timeout_tracker();
        new
    }

    fn start_timeout_tracker(&self) {
        let timeout_tracker = self.timeout_tracker.clone();
        let config = self.config.clone();
        tokio::spawn(async move {
            loop {
                tokio::time::sleep(Duration::from_millis(100)).await;
                let expired_ids = timeout_tracker.remove_expired();
                for id in expired_ids {
                    log::info!("Request timeout for component {}.", id);
                    config.reset_power_active(id).unwrap();
                }
            }
        });
    }
}

#[tonic::async_trait]
impl microgrid_server::Microgrid for MicrogridServer {
    type ReceiveElectricalComponentTelemetryStreamStream = Pin<
        Box<
            dyn Stream<
                    Item = Result<ReceiveElectricalComponentTelemetryStreamResponse, tonic::Status>,
                > + Send,
        >,
    >;
    type ReceiveSensorTelemetryStreamStream = Pin<
        Box<dyn Stream<Item = Result<ReceiveSensorTelemetryStreamResponse, tonic::Status>> + Send>,
    >;

    type SetElectricalComponentPowerStream = Pin<
        Box<dyn Stream<Item = Result<SetElectricalComponentPowerResponse, tonic::Status>> + Send>,
    >;

    async fn get_microgrid(
        &self,
        _request: tonic::Request<()>,
    ) -> std::result::Result<tonic::Response<GetMicrogridResponse>, tonic::Status> {
        let metadata = self.config.metadata().unwrap();
        Ok(tonic::Response::new(metadata))
    }

    async fn list_electrical_components(
        &self,
        _request: tonic::Request<ListElectricalComponentsRequest>,
    ) -> std::result::Result<tonic::Response<ListElectricalComponentsResponse>, tonic::Status> {
        let request = _request.into_inner();
        let components = self.config.components(request).unwrap();
        Ok(tonic::Response::new(components))
    }

    async fn list_electrical_component_connections(
        &self,
        _request: tonic::Request<ListElectricalComponentConnectionsRequest>,
    ) -> std::result::Result<
        tonic::Response<ListElectricalComponentConnectionsResponse>,
        tonic::Status,
    > {
        let request = _request.into_inner();
        let connections = self.config.connections(request).unwrap();
        Ok(tonic::Response::new(connections))
    }

    async fn set_electrical_component_power(
        &self,
        request: tonic::Request<SetElectricalComponentPowerRequest>,
    ) -> std::result::Result<tonic::Response<Self::SetElectricalComponentPowerStream>, tonic::Status>
    {
        let request = request.into_inner();
        let Ok(power_type) = PowerType::try_from(request.power_type) else {
            return Err(tonic::Status::invalid_argument(format!(
                "Invalid power type: {}",
                request.power_type
            )));
        };
        let res = match power_type {
            PowerType::Unspecified => {
                return Err(tonic::Status::invalid_argument(
                    "Power type cannot be UNSPECIFIED.",
                ));
            }
            PowerType::Active => self
                .config
                .set_power_active(request.electrical_component_id, request.power),
            PowerType::Reactive => self
                .config
                .set_power_reactive(request.electrical_component_id, request.power),
        };

        if let Err(err) = res {
            log::error!("Tulisp error:\n{}", err.format(&self.config.ctx.borrow()));
            return Err(tonic::Status::failed_precondition(err.desc()));
        }

        // TODO: when to reset? after latest request, or after older of the two
        // power types?
        let duration = if let Some(dur) = request.request_lifetime {
            if dur < 10 || dur > 60 * 15 {
                return Err(tonic::Status::invalid_argument(
                    "Request lifetime must be between 10 seconds and 15 minutes.",
                ));
            }
            Duration::from_secs(dur)
        } else {
            self.config.retain_requests_duration()
        };
        self.timeout_tracker
            .add(request.electrical_component_id, duration);

        let (tx, rx) = tokio::sync::mpsc::channel(1);
        let output_stream = ReceiverStream::new(rx);

        tokio::spawn(async move {
            if let Err(e) = tx
                .send(Ok(SetElectricalComponentPowerResponse {
                    valid_until_time: None,
                    status: SetElectricalComponentPowerRequestStatus::Success as i32,
                }))
                .await
            {
                log::error!("Failed to send SetElectricalComponentPowerResponse: {e}");
            }
        });

        Ok(tonic::Response::new(
            Box::pin(output_stream) as Self::SetElectricalComponentPowerStream
        ))
    }

    async fn receive_electrical_component_telemetry_stream(
        &self,
        request: tonic::Request<ReceiveElectricalComponentTelemetryStreamRequest>,
    ) -> std::result::Result<
        tonic::Response<Self::ReceiveElectricalComponentTelemetryStreamStream>,
        tonic::Status,
    > {
        let component_id = request.into_inner().electrical_component_id;

        let (tx, rx) = tokio::sync::mpsc::channel(128);
        let config = self.config.clone();

        tokio::spawn(async move {
            let mut last_msg_ts = SystemTime::now();
            loop {
                let (data, interval) = config
                    .get_component_data(component_id as u64)
                    .map_err(|e| {
                        log::error!("Tulisp error:\n{}", e.format(&config.ctx.borrow()));
                        e
                    })
                    .unwrap();

                if let Err(err) = tx.send(Result::<_, tonic::Status>::Ok(data)).await {
                    log::debug!("stream_component_data(component_id={component_id}): {err}");
                    break;
                }

                let now = SystemTime::now();
                let tgt_ts = last_msg_ts + Duration::from_millis(interval as u64);
                let dur =
                    Duration::from_millis(tgt_ts.duration_since(now).unwrap().as_millis() as u64);
                tokio::time::sleep(dur).await;
                last_msg_ts = tgt_ts;
            }
        });

        let output_stream = ReceiverStream::new(rx);
        Ok(tonic::Response::new(
            Box::pin(output_stream) as Self::ReceiveElectricalComponentTelemetryStreamStream
        ))
    }

    async fn augment_electrical_component_bounds(
        &self,
        request: tonic::Request<AugmentElectricalComponentBoundsRequest>,
    ) -> std::result::Result<tonic::Response<AugmentElectricalComponentBoundsResponse>, tonic::Status>
    {
        let request = request.into_inner();
        let component_id = request.electrical_component_id;
        let Ok(target_metric) = Metric::try_from(request.target_metric) else {
            return Err(tonic::Status::invalid_argument(format!(
                "Invalid metric type: {}",
                request.target_metric
            )));
        };

        if target_metric != Metric::AcPowerActive {
            return Err(tonic::Status::invalid_argument(format!(
                "Unsupported metric type: {}. Only AC_POWER_ACTIVE is supported.",
                request.target_metric
            )));
        }

        self.config
            .augment_active_power_bounds(component_id, request.bounds)
            .map_err(|e| {
                log::error!("Tulisp error:\n{}", e.format(&self.config.ctx.borrow()));
                tonic::Status::failed_precondition(e.desc())
            })?;

        Ok(tonic::Response::new(
            AugmentElectricalComponentBoundsResponse {
                valid_until_time: None,
            },
        ))
    }

    //
    //
    // Unused methods
    //
    //
    async fn list_sensors(
        &self,
        _request: tonic::Request<ListSensorRequest>,
    ) -> std::result::Result<tonic::Response<ListSensorsResponse>, tonic::Status> {
        todo!()
    }
    async fn receive_sensor_telemetry_stream(
        &self,
        _request: tonic::Request<ReceiveSensorTelemetryStreamRequest>,
    ) -> std::result::Result<tonic::Response<Self::ReceiveSensorTelemetryStreamStream>, tonic::Status>
    {
        todo!()
    }
    async fn start_electrical_component(
        &self,
        _request: tonic::Request<StartElectricalComponentRequest>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn put_electrical_component_in_standby(
        &self,
        _request: tonic::Request<PutElectricalComponentInStandbyRequest>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn stop_electrical_component(
        &self,
        _request: tonic::Request<StopElectricalComponentRequest>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
    async fn ack_electrical_component_error(
        &self,
        _request: tonic::Request<AckElectricalComponentErrorRequest>,
    ) -> std::result::Result<tonic::Response<()>, tonic::Status> {
        todo!()
    }
}
