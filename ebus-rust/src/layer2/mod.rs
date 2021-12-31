use std::fmt;

mod crc;

const EBUS_SYN: u8 = 0xaa;
const EBUS_ESCAPE: u8 = 0xa9;
const EBUS_ACKOK: u8 = 0x00;

#[derive(Default)]
pub struct Packet {
    pub source: u8,
    pub destination: u8,
    pub primary: u8,
    pub secondary: u8,
    payload_length: u8,
    payload: Vec<u8>,
    pub crc: u8,
    payload_slave_length: u8,
    payload_slave: Vec<u8>,
    crc_slave: u8,
}

impl Packet {
    // payload returns the un-escaped payload
    pub fn payload_decoded(&self) -> Vec<u8> {
        let mut v = Vec::new();
        let mut i = 0;

        while i < self.payload.len() {
            let c = self.payload[i];
            if c == EBUS_ESCAPE && i + 1 < self.payload.len() {
                i += 1;
                let c = self.payload[i];
                if c == 0x0 {
                    v.push(EBUS_ESCAPE);
                } else if c == 0x1 {
                    v.push(EBUS_SYN);
                } else {
                    v.push(c);
                }
            } else {
                v.push(c);
            }
            i += 1;
        }
        v
    }

    pub fn calc_crc(&self) -> u8 {
        let x = &[
            self.source,
            self.destination,
            self.primary,
            self.secondary,
            self.payload_length,
        ];
        crc::crc(&[&x[..], self.payload.as_slice()].concat())
    }
}

impl fmt::Debug for Packet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.payload_slave_length > 0 {
            f.debug_struct("Packet")
                .field("source", &format!("{:#02x}", &self.source))
                .field("destination", &format!("{:#02x}", &self.destination))
                .field("primary", &format!("{:#02x}", &self.primary))
                .field("secondary", &format!("{:#02x}", &self.secondary))
                .field("payload", &format!("{:02x?}", &self.payload))
                .field("payload_length", &self.payload_length)
                .field("crc", &format!("{:#02x}", &self.crc))
                .field(
                    "payload_slave_length",
                    &format!("{:#02x}", &self.payload_slave_length),
                )
                .field("payload_slave", &format!("{:02x?}", &self.payload_slave))
                .field("crc_slave", &format!("{:#02x}", &self.crc_slave))
                .finish()
        } else {
            f.debug_struct("Packet")
                .field("source", &format!("{:#02x}", &self.source))
                .field("destination", &format!("{:#02x}", &self.destination))
                .field("primary", &format!("{:#02x}", &self.primary))
                .field("secondary", &format!("{:#02x}", &self.secondary))
                .field("payload", &format!("{:02x?}", &self.payload))
                .field("payload_length", &self.payload_length)
                .field("crc", &format!("{:#02x}", &self.crc))
                .finish()
        }
    }
}

fn read_header(data: &[u8]) -> nom::IResult<&[u8], Packet> {
    use nom::number::streaming::u8;
    use nom::sequence::tuple;
    let (input, (source, destination, primary, secondary, payload_length)) =
        tuple((u8, u8, u8, u8, u8))(data)?;
    Ok((
        input,
        Packet {
            source,
            destination,
            primary,
            secondary,
            payload_length,
            ..Default::default()
        },
    ))
}

fn read_packet(data: &[u8]) -> nom::IResult<&[u8], Packet> {
    use nom::bytes::streaming::tag;
    use nom::combinator::opt;
    use nom::multi::count;
    use nom::number::streaming::u8;
    use nom::sequence::tuple;

    let (input, mut packet) = read_header(data)?;

    let (input2, (payload, crc, _, payload_slave_length)) = tuple((
        count(u8, packet.payload_length as usize), // payload
        u8,                                        // crc
        opt(tag([EBUS_ACKOK])),                    // ACK but non-ack is tolerated
        u8,                                        // SYN or payload slave length
    ))(input)?;
    packet.payload = payload;
    packet.crc = crc;

    if payload_slave_length != EBUS_SYN {
        packet.payload_slave_length = payload_slave_length;
        let (input3, (payload_slave, crc_slave)) =
            tuple((count(u8, payload_slave_length as usize), u8))(input2)?;
        packet.payload_slave = payload_slave;
        packet.crc_slave = crc_slave;
        Ok((input3, packet))
    } else {
        Ok((input2, packet))
    }
}

pub fn parse(data: &[u8]) -> Result<Packet, nom::Err<nom::error::Error<&[u8]>>> {
    use nom::bytes::streaming::take_while;
    use nom::sequence::preceded;
    fn take_syn(data: &[u8]) -> nom::IResult<&[u8], &[u8]> {
        take_while(|chr| chr == EBUS_SYN)(data)
    }

    let mut parser = preceded(take_syn, read_packet);
    let result = parser(data);
    result.map(|r| r.1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn teststructure_mastermaster() {
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

        let res = parse(&data).unwrap();
        assert_eq!(res.source, 0003);
        assert_eq!(res.destination, 241);
        assert_eq!(res.primary, 008);
        assert_eq!(res.secondary, 000);
        assert_eq!(res.payload_length, 0008);
        assert_eq!(res.crc, 128);
        assert_eq!(res.payload, vec![128, 040, 230, 002, 000, 002, 000, 010]);
    }

    #[test]
    fn testcrc() {
        let packets = [
            &[
                0x10, 0x03, 0x08, 0x00, 0x08, 0x00, 0x05, 0x00, 0x13, 0x80, 0x40, 0x00, 0x0a, 0x71,
                0x00, 0xaa,
            ][..],
            &[
                0x03, 0xf1, 0x08, 0x00, 0x08, 0x00, 0x14, 0x00, 0x13, 0x80, 0x00, 0x00, 0x0f, 0xc7,
                0x00, 0xaa,
            ][..],
            &[
                0x10, 0x03, 0x05, 0x07, 0x09, 0x00, 0x01, 0x50, 0x00, 0x01, 0x00, 0xff, 0x14, 0xff,
                0xa6, 0x00, 0xaa,
            ][..],
            &[
                0xf1, 0xfe, 0x05, 0x03, 0x08, 0x01, 0x00, 0x40, 0xff, 0x30, 0xff, 0x00, 0x13, 0xd8,
                0x00, 0xaa,
            ][..],
            &[
                0x03, 0xfe, 0x05, 0x03, 0x08, 0x01, 0x00, 0x00, 0x00, 0x30, 0x17, 0x33, 0x13, 0x82,
                0x00, 0xaa,
            ][..],
            &[
                // 00000d60: aaaa 1003 0507 09bb 044b 0300 80ff 54ff  .........K....T.
                // 00000d70: 0400 aaaa aaaa aaaa f1fe 0503 0801 0110  ................
                0x10, 0x03, 0x05, 0x07, 0x09, 0xbb, 0x04, 0x4b, 0x03, 0x00, 0x80, 0xff, 0x54, 0xff,
                0x04, 0x00, 0xaa,
            ][..],
        ];
        for d in &packets {
            let p = parse(d).unwrap();
            assert_eq!(p.crc, p.calc_crc(), "{:?}", p);
        }
    }

    #[test]
    fn test_escape() {
        let data: Vec<u8> = vec![
            170,         // Syn
            170,         // Syn
            3,           // Source
            241,         // Destination
            8,           // primaryCommand
            0,           // secondaryCommand
            8,           // payloadLength
            128,         // p1
            40,          // p2
            EBUS_ESCAPE, // p3
            1,           // p4
            EBUS_ESCAPE, // p5
            0,           // p6
            0,           // p7
            10,          // p8
            128,         // CRC
            0,           // ACK
            170,         // SYN
            170,
        ];

        let res = parse(&data).unwrap();
        assert_eq!(res.source, 0003);
        assert_eq!(res.destination, 241);
        assert_eq!(res.primary, 008);
        assert_eq!(res.secondary, 000);
        assert_eq!(res.payload_length, 0008);
        assert_eq!(res.crc, 128);
        assert_eq!(
            res.payload_decoded(),
            vec![128, 040, EBUS_SYN, EBUS_ESCAPE, 000, 010]
        );
    }
}
