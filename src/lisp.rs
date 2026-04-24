mod bounds;
mod time;

use crate::lisp::bounds::TulispComponentBounds;
use chrono::{DateTime, TimeDelta, Utc};
use rand::Rng;
use std::{collections::HashMap, path::Path, str::FromStr, sync::Arc, time::Duration};

use tulisp::SharedMut;

use crate::{
    lisp::bounds::VecBounds,
    lisp::time::TulispDateTime,
    proto::{
        common::{
            grid::{DeliveryArea, EnergyMarketCodeType},
            metrics::{
                Bounds, Metric, MetricSample, MetricValueVariant, SimpleMetricValue,
                metric_value_variant,
            },
            microgrid::{
                MicrogridStatus,
                electrical_components::{
                    Battery, BatteryType, ElectricalComponent, ElectricalComponentCategory,
                    ElectricalComponentCategorySpecificInfo, ElectricalComponentConnection,
                    ElectricalComponentStateCode, ElectricalComponentStateSnapshot,
                    ElectricalComponentTelemetry, EvCharger, EvChargerType, GridConnectionPoint,
                    Inverter, InverterType, MetricConfigBounds,
                    electrical_component_category_specific_info::Kind,
                },
            },
        },
        microgrid::{
            GetMicrogridResponse, ListElectricalComponentConnectionsRequest,
            ListElectricalComponentConnectionsResponse, ListElectricalComponentsRequest,
            ListElectricalComponentsResponse, ReceiveElectricalComponentTelemetryStreamResponse,
        },
    },
};
use notify::{RecommendedWatcher, Watcher};
use prost_types::Timestamp;
use tulisp::{Error, TulispContext, TulispConvertible, TulispObject, intern, list};

type CompDataMaker = fn(
    &mut TulispContext,
    &TulispObject,
    &Symbols,
) -> Result<ReceiveElectricalComponentTelemetryStreamResponse, Error>;

intern! {
    #[derive(Clone)]
    pub(crate) struct Symbols {
        reactive_power: "reactive-power",
        power: "power",
        name: "name",
        id: "id",
        soc: "soc",
        data: "data",
        type_: "type",
        status: "status",
        stream: "stream",
        bounds: "bounds",
        voltage: "voltage",
        current: "current",
        category: "category",
        interval: "interval",
        capacity: "capacity",
        location: "location",
        metadata: "metadata",
        soc_lower: "soc-lower",
        soc_upper: "soc-upper",
        relay_state: "relay-state",
        cable_state: "cable-state",
        socket_addr: "socket-addr",
        ac_frequency: "ac-frequency",
        component_state: "component-state",
        set_power_reactive: "set-power-reactive",
        enterprise_id: "enterprise-id",
        microgrid_id: "microgrid-id",
        rated_lower: "rated-lower",
        rated_upper: "rated-upper",
        delivery_area: "delivery-area",
        inclusion_lower: "inclusion-lower",
        inclusion_upper: "inclusion-upper",
        exclusion_lower: "exclusion-lower",
        exclusion_upper: "exclusion-upper",
        per_phase_power: "per-phase-power",
        components_alist: "components-alist",
        set_power_active: "set-power-active",
        create_timestamp: "create-timestamp",
        connections_alist: "connections-alist",
        rated_fuse_current: "rated-fuse-current",
        reset_power_active: "reset-power-active",
        per_phase_reactive_power: "per-phase-reactive-power",
        augment_active_power_bounds: "augment-active-power-bounds",
        retain_requests_duration_ms: "retain-requests-duration-ms",
    }
}

#[derive(Clone)]
pub struct Config {
    filename: String,

    pub(crate) ctx: SharedMut<tulisp::TulispContext>,

    /// Component ID -> (Component's Data Method, Interval, To ComponentData Method)
    stream_methods: SharedMut<HashMap<u64, (TulispObject, u64, CompDataMaker)>>,

    default_request_duration: Arc<std::sync::OnceLock<Duration>>,

    symbols: Symbols,
}

macro_rules! alist_get_as {
    ($ctx: expr, $rest:expr, $key:expr, $as_fn:ident) => {{ alist_get_as!($ctx, $rest, $key).and_then(|x| x.$as_fn()) }};
    ($ctx: expr, $rest:expr, $key:expr, eval++$as_fn:ident) => {{
        let out = alist_get_as!($ctx, $rest, $key);
        out.and_then(|x| $ctx.eval_and_then(&x, |_, x| x.$as_fn()))
    }};
    ($ctx: expr, $rest:expr, $key:expr) => {{ tulisp::lists::alist_get($ctx, $key, $rest, None, None, None) }};
}

macro_rules! alist_get_f32 {
    ($ctx: expr, $rest:expr, $key:expr) => {
        alist_get_as!($ctx, $rest, $key, eval ++ try_float).unwrap_or_default() as f32
    };
}

macro_rules! alist_get_u32 {
    ($ctx: expr, $rest:expr, $key:expr) => {
        alist_get_as!($ctx, $rest, $key, eval ++ try_int).unwrap_or_default() as u32
    };
}

macro_rules! alist_get_3_phase {
    ($ctx: expr, $rest:expr, $key:expr) => {{
        let expr = alist_get_as!($ctx, $rest, $key).unwrap_or_default();
        let items = if expr.consp() && expr.car_and_then(|x| Ok(x.numberp()))? {
            expr
        } else {
            $ctx.eval(&expr)?
        };
        (
            items
                .car_and_then(|x| $ctx.eval_and_then(&x, |_, x| x.as_float()))
                .unwrap_or_default() as f32,
            items
                .cadr_and_then(|x| $ctx.eval_and_then(&x, |_, x| x.as_float()))
                .unwrap_or_default() as f32,
            items
                .caddr_and_then(|x| $ctx.eval_and_then(&x, |_, x| x.as_float()))
                .unwrap_or_default() as f32,
        )
    }};
}

fn enum_from_alist<T: FromStr + Default>(
    ctx: &mut TulispContext,
    alist: &TulispObject,
    key: &TulispObject,
    eval: bool,
) -> Option<T> {
    let val = if eval {
        alist_get_as!(ctx, alist, key, eval ++ as_symbol).ok()?
    } else {
        alist_get_as!(ctx, alist, key, as_symbol).ok()?
    };
    match val.parse::<T>() {
        Ok(x) => Some(x),
        Err(_) => {
            log::error!("Invalid value for {}: {}", key, val);
            None
        }
    }
}

fn make_component_from_alist(
    ctx: &mut TulispContext,
    alist: &TulispObject,
    symbols: &Symbols,
) -> Result<ElectricalComponent, Error> {
    let id = alist_get_as!(ctx, alist, &symbols.id, as_int)? as u64;
    let name = alist_get_as!(ctx, alist, &symbols.name, as_string).unwrap_or_default();
    let Some(category) =
        enum_from_alist::<ElectricalComponentCategory>(ctx, alist, &symbols.category, false)
    else {
        return Err(Error::invalid_argument(format!(
            "Invalid component category for component {}",
            id
        )));
    };

    let kind = match category {
        ElectricalComponentCategory::Inverter => Some(Kind::Inverter(Inverter {
            r#type: enum_from_alist::<InverterType>(ctx, alist, &symbols.type_, false)
                .map(|typ| typ as i32)
                .unwrap_or_default(),
        })),
        ElectricalComponentCategory::Battery => Some(Kind::Battery(Battery {
            r#type: enum_from_alist::<BatteryType>(ctx, alist, &symbols.type_, false)
                .map(|typ| typ as i32)
                .unwrap_or_default(),
        })),
        ElectricalComponentCategory::EvCharger => Some(Kind::EvCharger(EvCharger {
            r#type: enum_from_alist::<EvChargerType>(ctx, alist, &symbols.type_, false)
                .map(|typ| typ as i32)
                .unwrap_or_default(),
        })),
        ElectricalComponentCategory::GridConnectionPoint => {
            Some(Kind::GridConnectionPoint(GridConnectionPoint {
                rated_fuse_current: alist_get_u32!(ctx, alist, &symbols.rated_fuse_current),
            }))
        }
        _ => None,
    };

    let rated_lower = alist_get_f32!(ctx, &alist, &symbols.rated_lower);
    let rated_upper = alist_get_f32!(ctx, &alist, &symbols.rated_upper);

    // Copy active bounds to reactive bounds.
    let reactive_upper = rated_lower.abs().max(rated_upper.abs());
    let reactive_lower = -reactive_upper;

    let comp = ElectricalComponent {
        id,
        name,
        category: category as i32,
        microgrid_id: 0, // TODO: Add microgrid_id
        category_specific_info: Some(ElectricalComponentCategorySpecificInfo { kind }),
        // status: todo!(),  // TODO: Add status
        // operational_lifetime: todo!(),
        metric_config_bounds: if category == ElectricalComponentCategory::Battery {
            vec![MetricConfigBounds {
                metric: Metric::DcPower as i32,
                config_bounds: Some(Bounds {
                    lower: Some(rated_lower),
                    upper: Some(rated_upper),
                }),
            }]
        } else {
            vec![
                MetricConfigBounds {
                    metric: Metric::AcPowerActive as i32,
                    config_bounds: Some(Bounds {
                        lower: Some(rated_lower),
                        upper: Some(rated_upper),
                    }),
                },
                MetricConfigBounds {
                    metric: Metric::AcPowerReactive as i32,
                    config_bounds: Some(Bounds {
                        lower: Some(reactive_lower),
                        upper: Some(reactive_upper),
                    }),
                },
            ]
        },
        ..Default::default()
    };

    Ok(comp)
}

impl Config {
    pub fn new(filename: &str) -> Self {
        let mut ctx = tulisp::TulispContext::new();

        let config_path = Path::new(filename);
        log::debug!("Using config path: {}", config_path.display());

        if let Some(p) = config_path.parent() {
            log::debug!("Using load path: {}", p.display());
            ctx.set_load_path(Some(p))
                .unwrap_or_else(|e| panic!("set_load_path({}): {:?}", p.display(), e));
        }

        add_functions(&mut ctx);

        tulisp_async::register(
            &mut ctx,
            Arc::new(tulisp_async::TokioExecutor::new()),
        );

        let _ = ctx.eval_file(filename).map_err(|e| {
            log::error!("Tulisp error:\n{}", e.format(&ctx));
            e
        });
        let symbols = Symbols::new(&mut ctx);
        Self {
            filename: filename.to_string(),
            ctx: SharedMut::new(ctx),
            stream_methods: SharedMut::new(HashMap::new()),
            default_request_duration: Arc::new(std::sync::OnceLock::new()),
            symbols,
        }
    }

    pub fn tags_table(filename: &str) -> Result<String, Error> {
        let mut ctx = tulisp::TulispContext::new();
        add_functions(&mut ctx);

        let config_path = Path::new(filename);
        log::debug!("Using config path: {}", config_path.display());

        if let Some(p) = config_path.parent() {
            log::debug!("Using load path: {}", p.display());
            ctx.set_load_path(Some(p))
                .unwrap_or_else(|e| panic!("set_load_path({}): {:?}", config_path.display(), e));
        }

        ctx.tags_table(Some(&[filename]))
    }

    pub fn reload(&self) {
        let start = std::time::Instant::now();
        let mut ctx = self.ctx.borrow_mut();
        if ctx
            .eval_file(&self.filename)
            .map_err(|e| {
                log::error!("Tulisp error:\n{}", e.format(&ctx));
                e
            })
            .is_err()
        {
            return;
        }
        let duration = start.elapsed();
        log::info!(
            "Reloaded config file in {}ms",
            duration.as_nanos() as f64 / 1e6
        );
        *self.stream_methods.borrow_mut() = HashMap::new();
    }

    pub async fn start(self) {
        self.start_watching().await;
    }

    async fn start_watching(self) {
        let (tx, mut rx) = tokio::sync::mpsc::channel(1);

        let mut watcher = RecommendedWatcher::new(
            move |res| {
                futures::executor::block_on(async {
                    tx.send(res).await.unwrap();
                });
            },
            notify::Config::default(),
        )
        .unwrap();
        watcher
            .watch(
                &Path::new(&self.filename),
                notify::RecursiveMode::NonRecursive,
            )
            .unwrap();

        while let Some(res) = rx.recv().await {
            match res {
                Ok(event) => {
                    if let notify::EventKind::Modify(_) = event.kind {
                        tokio::time::sleep(Duration::from_millis(50)).await;
                        self.reload();
                    }
                }
                Err(e) => {
                    log::error!("watch error: {:?}", e);
                    return;
                }
            }
        }
    }

    pub fn socket_addr(&self) -> String {
        let addr = self.symbols.socket_addr.get().and_then(|x| x.as_string());

        match addr {
            Ok(vv) => vv,
            Err(err) => {
                panic!(
                    r#"{}

Invalid socket-addr.  Add a config line in this format:
	(setq socket-addr "[::1]:8080")
"#,
                    err.format(&self.ctx.borrow())
                )
            }
        }
    }

    pub fn register_log_buffer(&self, buffer: crate::tui_log::LogBuffer) {
        self.ctx.borrow_mut().defun("tui/log-lines", move || {
            Ok::<_, Error>(buffer.lines())
        });
    }

    pub async fn run_tui(&self) -> Result<(), Error> {
        let frame_fn = self.ctx.borrow_mut().intern("tui/frame");
        let args = TulispObject::nil();

        // ~60 fps; also the window during which other ctx writers
        // (grpc handlers like set-power-active) can acquire the lock.
        // `yield_now` alone doesn't buy enough headroom here —
        // `std::sync::RwLock` gives no writer-fairness guarantee, so a
        // re-queued TUI task tends to re-acquire before another writer's
        // waker runs, starving it.
        let frame_period = Duration::from_millis(16);

        let result = loop {
            let res = self.ctx.borrow_mut().funcall(&frame_fn, &args);
            match res {
                Ok(v) if !v.null() => break Ok(()),
                Ok(_) => {}
                Err(e) => {
                    log::error!("tui error: {}", e.format(&self.ctx.borrow()));
                    break Err(e);
                }
            }
            tokio::time::sleep(frame_period).await;
        };

        tulisp_ratatui::restore();
        result
    }

    pub fn retain_requests_duration(&self) -> Duration {
        *self.default_request_duration.get_or_init(|| {
            let dur_ms = self
                .symbols
                .retain_requests_duration_ms
                .get()
                .and_then(|x| x.as_int())
                .unwrap_or(5000);
            Duration::from_millis(dur_ms as u64)
        })
    }

    pub fn metadata(&self) -> Result<GetMicrogridResponse, Error> {
        let alist = self
            .symbols
            .metadata
            .get()
            .unwrap_or_else(|_| TulispObject::nil());

        let microgrid_id = alist_get_as!(
            &mut self.ctx.borrow_mut(),
            &alist,
            &self.symbols.microgrid_id,
            as_int
        )
        .unwrap_or_default() as u64;

        let enterprise_id = alist_get_as!(
            &mut self.ctx.borrow_mut(),
            &alist,
            &self.symbols.enterprise_id,
            as_int
        )
        .unwrap_or_default() as u64;

        let delivery_area = if let Ok(delivery_area) = alist_get_as!(
            &mut self.ctx.borrow_mut(),
            &alist,
            &self.symbols.delivery_area
        ) {
            Some(DeliveryArea {
                code: delivery_area.car()?.as_string().unwrap_or_default(),
                code_type: delivery_area
                    .cadr()?
                    .as_symbol()?
                    .parse::<EnergyMarketCodeType>()
                    .unwrap_or_default() as i32,
            })
        } else {
            None
        };

        let location = if let Ok(location) =
            alist_get_as!(&mut self.ctx.borrow_mut(), &alist, &self.symbols.location)
        {
            Some(crate::proto::common::types::Location {
                latitude: location.car()?.as_float().unwrap_or_default() as f32,
                longitude: location.cadr()?.as_float().unwrap_or_default() as f32,
                country_code: location.caddr()?.as_string().unwrap_or_default(),
            })
        } else {
            None
        };

        let status = alist_get_as!(
            &mut self.ctx.borrow_mut(),
            &alist,
            &self.symbols.status,
            as_symbol
        )
        .unwrap_or_default()
        .parse::<MicrogridStatus>()
        .unwrap_or_default() as i32;

        let create_timestamp = if let Ok(iso_ts) = alist_get_as!(
            &mut self.ctx.borrow_mut(),
            &alist,
            &self.symbols.create_timestamp,
            as_string
        ) {
            Some(Timestamp::from_str(iso_ts.as_str()).unwrap_or_default())
        } else {
            None
        };

        Ok(GetMicrogridResponse {
            microgrid: Some(crate::proto::common::microgrid::Microgrid {
                id: microgrid_id,
                enterprise_id,
                name: format!("Microgrid {}", microgrid_id),
                delivery_area,
                location,
                status,
                create_timestamp,
            }),
        })
    }

    pub fn components(
        &self,
        request: ListElectricalComponentsRequest,
    ) -> Result<ListElectricalComponentsResponse, Error> {
        let alists = self.symbols.components_alist.get()?;

        Ok(ListElectricalComponentsResponse {
            electrical_components: alists
                .base_iter()
                .map(|x| {
                    make_component_from_alist(&mut self.ctx.borrow_mut(), &x, &self.symbols)
                        .unwrap()
                })
                .filter(|x| {
                    (request.electrical_component_ids.contains(&x.id)
                        || request.electrical_component_ids.is_empty())
                        && (request
                            .electrical_component_categories
                            .contains(&x.category)
                            || request.electrical_component_categories.is_empty())
                })
                .collect(),
        })
    }

    pub fn connections(
        &self,
        request: ListElectricalComponentConnectionsRequest,
    ) -> Result<ListElectricalComponentConnectionsResponse, Error> {
        let alist = self.symbols.connections_alist.get()?;
        Ok(ListElectricalComponentConnectionsResponse {
            electrical_component_connections: alist
                .base_iter()
                .map(|x| ElectricalComponentConnection {
                    source_electrical_component_id: x.car().and_then(|x| x.as_int()).unwrap()
                        as u64,
                    destination_electrical_component_id: x.cdr().and_then(|x| x.as_int()).unwrap()
                        as u64,
                    ..Default::default()
                })
                .filter(|x| {
                    (request
                        .source_electrical_component_ids
                        .contains(&x.source_electrical_component_id)
                        || request.source_electrical_component_ids.is_empty())
                        && (request
                            .destination_electrical_component_ids
                            .contains(&x.destination_electrical_component_id)
                            || request.destination_electrical_component_ids.is_empty())
                })
                .collect(),
        })
    }

    pub fn set_power_active(&self, component_id: u64, power: f32) -> Result<(), Error> {
        let res = self.ctx.borrow_mut().funcall(
            &self.symbols.set_power_active,
            &list![(component_id as i64).into(), (power as f64).into()]?,
        )?;

        // TODO: use throw from tulisp to return errors
        if !res.null() {
            return Err(Error::lisp_error(res.as_string()?).with_trace(res));
        }
        Ok(())
    }

    pub fn set_power_reactive(&self, component_id: u64, power: f32) -> Result<(), Error> {
        let res = self.ctx.borrow_mut().funcall(
            &self.symbols.set_power_reactive,
            &list![(component_id as i64).into(), (power as f64).into()]?,
        )?;

        // TODO: use throw from tulisp to return errors
        if !res.null() {
            return Err(Error::lisp_error(res.as_string()?).with_trace(res));
        }
        Ok(())
    }

    pub fn reset_power_active(&self, component_id: u64) -> Result<(), Error> {
        #[inline(always)]
        fn work(config: &Config, component_id: u64) -> Result<(), Error> {
            config.ctx.borrow_mut().funcall(
                &config.symbols.reset_power_active,
                &list![(component_id as i64).into()]?,
            )?;

            Ok(())
        }
        work(self, component_id)
            .inspect_err(|e| log::error!("Tulisp error:\n{}", e.format(&self.ctx.borrow())))
    }

    pub fn augment_active_power_bounds(
        &self,
        component_id: u64,
        bounds: Vec<Bounds>,
        request_lifetime_s: i64,
    ) -> Result<Option<DateTime<Utc>>, Error> {
        if bounds.is_empty() {
            return Ok(None);
        }

        if bounds.len() > 1 {
            return Err(Error::invalid_argument(format!(
                "Only one phase is supported for bounds augmentation, but got {}",
                bounds.len()
            )));
        }

        let create_time = TulispDateTime::now();

        self.ctx.borrow_mut().funcall(
            &self.symbols.augment_active_power_bounds,
            &list![
                ,(component_id as i64).into_tulisp()
                ,create_time.into_tulisp()
                ,VecBounds::new(bounds).into_tulisp()
                ,request_lifetime_s.into_tulisp()
            ]?,
        )?;

        let expiry_time = *create_time + TimeDelta::seconds(request_lifetime_s);

        Ok(Some(expiry_time))
    }

    fn get_conv_function(&self, component_id: u64, comp: &TulispObject) -> CompDataMaker {
        match make_component_from_alist(&mut self.ctx.borrow_mut(), &comp, &self.symbols)
            .unwrap()
            .category()
        {
            ElectricalComponentCategory::Battery => Self::battery_data,
            ElectricalComponentCategory::Inverter => Self::inverter_data,
            ElectricalComponentCategory::Meter => Self::meter_data,
            ElectricalComponentCategory::EvCharger => Self::ev_charger_data,
            _ => Err(Error::invalid_argument(format!(
                "Invalid component category for component {}",
                component_id
            )))
            .unwrap(),
        }
    }

    pub fn get_component_data(
        &self,
        component_id: u64,
    ) -> Result<(ReceiveElectricalComponentTelemetryStreamResponse, u64), Error> {
        let mut stream_methods = self.stream_methods.borrow_mut();
        let (data_method, interval, conv_function) =
            if let Some((data_method, interval, conv_function)) = stream_methods.get(&component_id)
            {
                (data_method.clone(), *interval, *conv_function)
            } else {
                let alists = self.symbols.components_alist.get()?;
                let comp = alists
                    .base_iter()
                    .find(|x| {
                        alist_get_as!(&mut self.ctx.borrow_mut(), &x, &self.symbols.id, as_int)
                            .unwrap() as u64
                            == component_id
                    })
                    .expect(&format!("Component id {component_id} not found"));

                let stream =
                    alist_get_as!(&mut self.ctx.borrow_mut(), &comp, &self.symbols.stream).unwrap();

                let interval = alist_get_as!(
                    &mut self.ctx.borrow_mut(),
                    &stream,
                    &self.symbols.interval,
                    as_int
                )
                .unwrap();
                let data_method =
                    alist_get_as!(&mut self.ctx.borrow_mut(), &stream, &self.symbols.data).unwrap();

                let conv_function = self.get_conv_function(component_id, &comp);

                stream_methods.insert(
                    component_id,
                    (data_method.clone(), interval as u64, conv_function),
                );

                (data_method, interval as u64, conv_function)
            };

        let tulisp_data = self
            .ctx
            .borrow_mut()
            .funcall(&data_method, &list!((component_id as i64).into())?);
        let tulisp_data = tulisp_data.map_err(|e| {
            log::error!("Tulisp error:\n{}", e.format(&self.ctx.borrow()));
            panic!();
        })?;

        let comp_data = conv_function(&mut self.ctx.borrow_mut(), &tulisp_data, &self.symbols);
        let comp_data = comp_data.map_err(|e| {
            log::error!("Tulisp error:\n{}", e.format(&self.ctx.borrow()));
            panic!();
        })?;

        Ok((comp_data, interval as u64))
    }
}

/// ComponentData methods
impl Config {
    fn battery_data(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<ReceiveElectricalComponentTelemetryStreamResponse, Error> {
        let id = alist_get_as!(ctx, &alist, &symbols.id, eval ++ as_int)? as u64;
        let capacity = alist_get_f32!(ctx, &alist, &symbols.capacity);

        let soc = alist_get_f32!(ctx, &alist, &symbols.soc);
        let soc_lower = alist_get_f32!(ctx, &alist, &symbols.soc_lower);
        let soc_upper = alist_get_f32!(ctx, &alist, &symbols.soc_upper);

        let voltage = alist_get_f32!(ctx, &alist, &symbols.voltage);
        let current = alist_get_f32!(ctx, &alist, &symbols.current);
        let power = alist_get_f32!(ctx, &alist, &symbols.power);

        let bounds: TulispComponentBounds = TulispConvertible::from_tulisp(
            &alist_get_as!(ctx, &alist, &symbols.bounds).and_then(|x| ctx.eval(&x))?,
        )?;

        let component_state = enum_from_alist::<ElectricalComponentStateCode>(
            ctx,
            &alist,
            &symbols.component_state,
            true,
        )
        .unwrap_or_default() as i32;
        let relay_state = enum_from_alist::<ElectricalComponentStateCode>(
            ctx,
            &alist,
            &symbols.relay_state,
            true,
        )
        .unwrap_or_default() as i32;

        let now = Some(Timestamp::from(std::time::SystemTime::now()));

        Ok(ReceiveElectricalComponentTelemetryStreamResponse {
            telemetry: Some(ElectricalComponentTelemetry {
                electrical_component_id: id,
                metric_samples: vec![
                    MetricSample {
                        sample_time: now,
                        metric: Metric::BatteryCapacity as i32,
                        value: Some(MetricValueVariant {
                            metric_value_variant: Some(
                                metric_value_variant::MetricValueVariant::SimpleMetric(
                                    SimpleMetricValue { value: capacity },
                                ),
                            ),
                        }),
                        ..Default::default() // TODO: Add bounds and states
                    },
                    MetricSample {
                        sample_time: now,
                        metric: Metric::BatterySocPct as i32,
                        value: Some(MetricValueVariant {
                            metric_value_variant: Some(
                                metric_value_variant::MetricValueVariant::SimpleMetric(
                                    SimpleMetricValue { value: soc },
                                ),
                            ),
                        }),
                        bounds: vec![Bounds {
                            lower: Some(soc_lower),
                            upper: Some(soc_upper),
                        }],
                        ..Default::default() // TODO: Add bounds and states
                    },
                    MetricSample {
                        sample_time: now,
                        metric: Metric::DcVoltage as i32,
                        value: Some(MetricValueVariant {
                            metric_value_variant: Some(
                                metric_value_variant::MetricValueVariant::SimpleMetric(
                                    SimpleMetricValue { value: voltage },
                                ),
                            ),
                        }),
                        ..Default::default() // TODO: Add bounds and states
                    },
                    MetricSample {
                        sample_time: now,
                        metric: Metric::DcCurrent as i32,
                        value: Some(MetricValueVariant {
                            metric_value_variant: Some(
                                metric_value_variant::MetricValueVariant::SimpleMetric(
                                    SimpleMetricValue { value: current },
                                ),
                            ),
                        }),
                        ..Default::default() // TODO: Add bounds and states
                    },
                    MetricSample {
                        sample_time: now,
                        metric: Metric::DcPower as i32,
                        value: Some(MetricValueVariant {
                            metric_value_variant: Some(
                                metric_value_variant::MetricValueVariant::SimpleMetric(
                                    SimpleMetricValue { value: power },
                                ),
                            ),
                        }),
                        bounds: bounds.squash().0,
                        ..Default::default() // TODO: Add bounds and states
                    },
                ],
                state_snapshots: vec![ElectricalComponentStateSnapshot {
                    origin_time: now,
                    states: vec![component_state, relay_state],
                    ..Default::default()
                }],
                ..Default::default()
            }),
        })
    }

    fn ac_from_alist(
        ctx: &mut TulispContext,
        now: Option<Timestamp>,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<Vec<MetricSample>, Error> {
        let frequency = symbols
            .ac_frequency
            .get()
            .and_then(|x| x.as_float())
            .unwrap_or_default() as f32;
        let current = alist_get_3_phase!(ctx, &alist, &symbols.current);
        let voltage = alist_get_3_phase!(ctx, &alist, &symbols.voltage);
        let per_phase_power = alist_get_3_phase!(ctx, &alist, &symbols.per_phase_power);
        let power = alist_get_f32!(ctx, &alist, &symbols.power);
        let per_phase_reactive_power =
            alist_get_3_phase!(ctx, &alist, &symbols.per_phase_reactive_power);
        let reactive_power = alist_get_f32!(ctx, &alist, &symbols.reactive_power);

        let bounds: Option<TulispComponentBounds> = TulispConvertible::from_tulisp(
            &alist_get_as!(ctx, &alist, &symbols.bounds).and_then(|x| ctx.eval(&x))?,
        )
        .ok();

        Ok(vec![
            MetricSample {
                sample_time: now,
                metric: Metric::AcFrequency as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: frequency,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcVoltagePhase1N as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: voltage.0,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcVoltagePhase2N as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: voltage.1,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcVoltagePhase3N as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: voltage.2,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcCurrentPhase1 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: current.0,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcCurrentPhase2 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: current.1,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcCurrentPhase3 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: current.2,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcPowerReactivePhase1 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: per_phase_reactive_power.0,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcPowerReactivePhase2 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: per_phase_reactive_power.1,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcPowerReactivePhase3 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: per_phase_reactive_power.2,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcPowerActivePhase1 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: per_phase_power.0,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcPowerActivePhase2 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: per_phase_power.1,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcPowerActivePhase3 as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: per_phase_power.2,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcPowerReactive as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: reactive_power,
                        }),
                    ),
                }),
                ..Default::default()
            },
            MetricSample {
                sample_time: now,
                metric: Metric::AcPowerActive as i32,
                value: Some(MetricValueVariant {
                    metric_value_variant: Some(
                        metric_value_variant::MetricValueVariant::SimpleMetric(SimpleMetricValue {
                            value: power,
                        }),
                    ),
                }),
                bounds: bounds.map(|b| b.squash().0).unwrap_or_default(),
                ..Default::default()
            },
        ])
    }

    fn inverter_data(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<ReceiveElectricalComponentTelemetryStreamResponse, Error> {
        let id = alist_get_as!(ctx, &alist, &symbols.id, eval ++ as_int)? as u64;

        let component_state = enum_from_alist::<ElectricalComponentStateCode>(
            ctx,
            &alist,
            &symbols.component_state,
            true,
        )
        .unwrap_or_default() as i32;

        let now = Some(Timestamp::from(std::time::SystemTime::now()));

        Ok(ReceiveElectricalComponentTelemetryStreamResponse {
            telemetry: Some(ElectricalComponentTelemetry {
                electrical_component_id: id,
                metric_samples: Self::ac_from_alist(ctx, now, alist, symbols)?,
                state_snapshots: vec![ElectricalComponentStateSnapshot {
                    origin_time: now,
                    states: vec![component_state],
                    ..Default::default()
                }],
                ..Default::default()
            }),
        })
    }

    fn meter_data(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<ReceiveElectricalComponentTelemetryStreamResponse, Error> {
        let id = alist_get_as!(ctx, &alist, &symbols.id, eval ++ as_int)? as u64;

        let now = Some(Timestamp::from(std::time::SystemTime::now()));

        Ok(ReceiveElectricalComponentTelemetryStreamResponse {
            telemetry: Some(ElectricalComponentTelemetry {
                electrical_component_id: id,
                metric_samples: Self::ac_from_alist(ctx, now, alist, symbols)?,
                state_snapshots: vec![ElectricalComponentStateSnapshot {
                    origin_time: now,
                    states: vec![
                        enum_from_alist::<ElectricalComponentStateCode>(
                            ctx,
                            &alist,
                            &symbols.component_state,
                            true,
                        )
                        .unwrap_or_default() as i32,
                    ],
                    ..Default::default()
                }],
                ..Default::default()
            }),
        })
    }

    fn ev_charger_data(
        ctx: &mut TulispContext,
        alist: &TulispObject,
        symbols: &Symbols,
    ) -> Result<ReceiveElectricalComponentTelemetryStreamResponse, Error> {
        let id = alist_get_as!(ctx, &alist, &symbols.id, eval ++ as_int)? as u64;

        let component_state = enum_from_alist::<ElectricalComponentStateCode>(
            ctx,
            &alist,
            &symbols.component_state,
            true,
        )
        .unwrap_or_default() as i32;

        let cable_state = enum_from_alist::<ElectricalComponentStateCode>(
            ctx,
            &alist,
            &symbols.cable_state,
            true,
        )
        .unwrap_or_default() as i32;

        let now = Some(Timestamp::from(std::time::SystemTime::now()));

        Ok(ReceiveElectricalComponentTelemetryStreamResponse {
            telemetry: Some(ElectricalComponentTelemetry {
                electrical_component_id: id,
                metric_samples: vec![],
                state_snapshots: vec![ElectricalComponentStateSnapshot {
                    origin_time: now,
                    states: vec![component_state, cable_state],
                    ..Default::default()
                }],
                ..Default::default()
            }),
        })
    }
}

fn add_functions(ctx: &mut TulispContext) {
    ctx.defun("log.info", |msg: String| log::info!("{msg}"))
        .defun("log.warn", |msg: String| log::warn!("{msg}"))
        .defun("log.error", |msg: String| log::error!("{msg}"))
        .defun("log.debug", |msg: String| log::debug!("{msg}"))
        .defun("log.trace", |msg: String| log::trace!("{msg}"))
        .defun("ceiling", |n: f64| n.ceil() as i64)
        .defun("floor", |n: f64| n.floor() as i64)
        .defun("random", |limit: Option<i64>| {
            if let Some(limit) = limit {
                rand::thread_rng().gen_range(0..limit)
            } else {
                rand::thread_rng().r#gen()
            }
        });

    crate::lisp::time::add(ctx);
    crate::lisp::bounds::add(ctx);
    tulisp_ratatui::register(ctx);
}
