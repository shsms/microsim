use common::{
    grid::EnergyMarketCodeType,
    microgrid::{MicrogridStatus, electrical_components::ElectricalComponentStateCode},
};

use crate::proto::common::microgrid::electrical_components::{
    BatteryType, ElectricalComponentCategory, EvChargerType, InverterType,
};

#[allow(
    clippy::doc_lazy_continuation,
    clippy::module_inception,
    dead_code,
    clippy::enum_variant_names
)]
mod pb {
    tonic::include_proto!("proto_v1_alpha18");
}

pub use pb::frequenz::api::common::v1alpha8 as common;
pub use pb::frequenz::api::microgrid::v1alpha18 as microgrid;

macro_rules! impl_enum_from_str {
    ($(($t:ty, $p:literal),)+) => {
        $(
            impl std::str::FromStr for $t {
                type Err = ();

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    let s = s.replace("-", "_");
                    match <$t>::from_str_name(($p.to_string() + &s).to_uppercase().as_str()) {
                        Some(x) => Ok(x),
                        None => Err(()),
                    }
                }
            }
        )+
    };
}

impl_enum_from_str!(
    (
        ElectricalComponentCategory,
        "ELECTRICAL_COMPONENT_CATEGORY_"
    ),
    (BatteryType, "BATTERY_TYPE_"),
    (InverterType, "INVERTER_TYPE_"),
    (EvChargerType, "EV_CHARGER_TYPE_"),
    (
        ElectricalComponentStateCode,
        "ELECTRICAL_COMPONENT_STATE_CODE_"
    ),
    (EnergyMarketCodeType, "ENERGY_MARKET_CODE_TYPE_"),
    (MicrogridStatus, "MICROGRID_STATUS_"),
);
