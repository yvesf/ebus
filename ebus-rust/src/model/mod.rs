use serde::Deserialize;

use quick_xml::de::{from_reader};

pub fn read_config(xml: &[u8]) -> Result<Ebus, String> {
    let ebus_configuration = from_reader(xml).map_err(|e| format!("failed to read xml: {}", e))?;
    Ok(ebus_configuration)
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Description {
    pub lang: String,
    #[serde(rename = "$value")]
    pub text: String,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum DeviceType {
    Master,
    Slave,
    Broadcast,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Device {
    pub address: u8,
    #[serde(rename = "type", default)]
    pub device_type: Option<DeviceType>,
    pub name: String,
    #[serde(rename = "description", default)]
    pub descriptions: Vec<Description>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Devices {
    #[serde(rename = "device", default)]
    pub devices: Vec<Device>,
}

#[derive(Debug, Deserialize, PartialEq, Clone)]
pub struct ByteEnumOption {
    pub value: u8,
    pub name: String,
}

#[derive(Debug, Deserialize, PartialEq, Clone)]
pub struct ByteEnumList(pub Vec<ByteEnumOption>);

impl ByteEnumList {
    pub fn get(&self, value: u8) -> Option<String> {
        self.0
            .iter()
            .find(|v| v.value == value)
            .map(|v| v.name.clone())
    }
}

#[derive(Debug, Deserialize, PartialEq, Clone)]
pub enum PacketField {
    #[serde(rename = "byte")]
    Byte { offset: u8, name: String },
    #[serde(rename = "data1b")]
    Data1b { offset: u8, name: String },
    #[serde(rename = "data1c")]
    Data1c { offset: u8, name: String },
    #[serde(rename = "data2b")]
    Data2b { offset: u8, name: String },
    #[serde(rename = "data2c")]
    Data2c { offset: u8, name: String },
    #[serde(rename = "byteEnum")]
    ByteEnum {
        offset: u8,
        name: String,
        #[serde(rename = "option")]
        options: ByteEnumList,
    },
    #[serde(rename = "bcd")]
    Bcd { offset: u8, name: String },
    #[serde(rename = "bit")]
    Bit { offset: u8, name: String },
    #[serde(rename = "word")]
    Word { offset: u8, name: String },
    #[serde(rename = "string")]
    String {
        offset: u8,
        name: String,
        length: u8,
    },
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct PacketFields {
    #[serde(rename = "$value")]
    pub fields: Option<Vec<PacketField>>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Packet {
    pub primary: u8,
    pub secondary: u8,
    pub name: String,
    #[serde(rename = "description", default)]
    pub descriptions: Vec<Description>,
    pub fields: PacketFields,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Packets {
    #[serde(rename = "packet", default)]
    pub packets: Vec<Packet>,
}

impl Packets {
    pub fn get(&self, primary: u8, secondary: u8) -> Option<&Packet> {
        self.packets
            .iter()
            .find(|p| p.primary == primary && p.secondary == secondary)
    }
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Ebus {
    pub devices: Devices,
    pub packets: Packets,
}
