pub mod types;
use crate::layer2;
use crate::model;

pub struct DecodedField {
    pub v: Option<types::Value>,
    pub name: String,
    pub field: model::PacketField,
}

pub fn decode_fields(p: &layer2::Packet, fields: &[model::PacketField]) -> Vec<DecodedField> {
    fields
        .iter()
        .map(|f| {
            let r = |decoder: &dyn Fn(&[u8]) -> Option<types::Value>,
                     offset: &u8,
                     name: &String| DecodedField {
                v: decoder(
                    p.payload_decoded()
                        .get((*offset as usize)..)
                        .unwrap_or(&[0; 0]),
                ),
                name: name.clone(),
                field: f.clone(),
            };
            match f {
                model::PacketField::Byte { offset, name } => r(&types::byte, offset, name),
                model::PacketField::Data1b { offset, name } => r(&types::data1b, offset, name),
                model::PacketField::Data1c { offset, name } => r(&types::data1c, offset, name),
                model::PacketField::Data2b { offset, name } => r(&types::data2b, offset, name),
                model::PacketField::Data2c { offset, name } => r(&types::data2c, offset, name),
                model::PacketField::ByteEnum {
                    offset,
                    name,
                    options,
                } => r(&(move |data| types::byteenum(data, options)), offset, name),
                model::PacketField::Bcd { offset, name } => r(&types::bcd, offset, name),
                model::PacketField::Bit { offset, name } => r(&types::bit, offset, name),
                model::PacketField::Word { offset, name } => r(&types::word, offset, name),
                model::PacketField::String {
                    offset,
                    name,
                    length,
                } => r(
                    &(move |data| types::string(data, *length as usize)),
                    offset,
                    name,
                ),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    static EBUS_XML: &[u8] = include_bytes!("../../../ebus-xml/ebus.xml");

    fn tostring(values: Vec<DecodedField>) -> String {
        values
            .iter()
            .map(|df| {
                df.name.clone()
                    + "="
                    + &df
                        .v
                        .as_ref()
                        .map(|v2| v2.convert())
                        .unwrap_or(String::from("<None>"))
            })
            .collect::<Vec<String>>()
            .join(",")
    }

    #[test]
    fn test1() {
        let conf = model::read_config(EBUS_XML).unwrap();
        let data = [
            0xf1, 0xfe, 0x5, 0x3, 0x08, 0x01, 0x01, 0x10, 0xff, 0x4e, 0xff, 0x35, 0x0b, 0x92, 0x00,
            0xaa,
        ];

        let l2p = layer2::parse(&data[..]).unwrap();
        let pack = conf.packets.get(l2p.primary, l2p.secondary).unwrap();
        let values = decode_fields(&l2p, pack.fields.fields.as_ref().unwrap());
        assert_eq!("stellgradKesselleistung=<None>,kesselTemperatur=39,ruecklaufTemperatur=<None>,boilerTemperatur=53,aussenTemperatur=11", tostring(values));
    }

    #[test]
    fn test() {
        let conf = model::read_config(EBUS_XML).unwrap();

        let data: Vec<u8> = vec![
            170, // Syn
            170, // Syn
            003, // Source
            241, // Destination
            008, // primaryCommand
            000, // secondaryCommand
            008, // payloadLength
            128, // p1
            040, // p2
            230, // p3
            002, // p4
            000, // p5
            002, // p6
            000, // p7
            010, // p8
            128, // CRC
            000, // ACK
            170, // SYN
            170,
        ];

        let res = layer2::parse(&data).unwrap();
        assert_eq!(
            res.payload_decoded(),
            vec![128, 040, 230, 002, 000, 002, 000, 010]
        );

        let pack = conf.packets.get(res.primary, res.secondary).unwrap();
        let values = decode_fields(&res, pack.fields.fields.as_ref().unwrap());
        assert_eq!(
            "TK_soll=40.5,TA_ist=2.8984375,L_zwang=0,Status=false,TB_soll=10",
            tostring(values)
        );
    }
}
