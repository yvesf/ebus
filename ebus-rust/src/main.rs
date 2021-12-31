extern crate bytes;
extern crate env_logger;
extern crate log;
extern crate nom;
extern crate quick_xml;
extern crate serde;
extern crate structopt;

mod layer2;
mod layer7;
mod model;

use bytes::{BufMut, BytesMut};
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::Path;
use std::thread;
use std::time;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opts {
    #[structopt(
        short,
        long,
        default_value = "",
        help = "if not set internal copy will be used"
    )]
    config: String,
    #[structopt(skip)]
    ebus_xml: Vec<u8>,
    #[structopt(short, long, help = "Use the adapter.ebusd.eu enhanced protocol")]
    enhanced: bool,
    #[structopt(subcommand)]
    subcmd: Command,
}

#[derive(Debug, StructOpt)]
enum Command {
    Dump(Dump),
    DumpL2(DumpL2),
    ParseL2(ParseL2),
    ParseL7(ParseL7),
    Influxdb(Influxdb),
}

impl Command {
    fn run(&self, opts: &Opts) {
        match self {
            Command::Dump(..) => dump(opts),
            Command::DumpL2(..) => dump_l2(opts),
            Command::ParseL2(..) => parse_l2(opts, &|p| println!("{:?}", p)),
            Command::ParseL7(..) => parse_l7(opts, &|_, _, _, _| ()),
            Command::Influxdb(cfg) => influxdb(opts, cfg),
        }
    }
}

#[derive(Debug, StructOpt)]
#[structopt(about = "Dump the configuration from XML to stdout")]
struct Dump {}

#[derive(Debug, StructOpt)]
#[structopt(about = "Dump raw data")]
struct DumpL2 {}

#[derive(Debug, StructOpt)]
#[structopt(about = "Parse and dump L2 from stdin")]
struct ParseL2 {}

#[derive(Debug, StructOpt)]
#[structopt(about = "Parse and dump L7 from stdin")]
struct ParseL7 {}

#[derive(Debug, StructOpt)]
#[structopt(about = "Parse from stdin and write to influxdb")]
struct Influxdb {
    #[structopt(short, long, default_value = "http://localhost:8086")]
    url: String,
    #[structopt(short, long, default_value = "ebus")]
    db: String,
    #[structopt(short, long, default_value = "ebus")]
    measurement: String,
}

fn main() {
    env_logger::init();
    let mut opts: Opts = Opts::from_args();

    log::info!("Enhanced protocol is status: {}", opts.enhanced);

    if opts.config.is_empty() {
        opts.ebus_xml = Vec::from(*include_bytes!("../../ebus-xml/ebus.xml"));
    } else {
        let path = Path::new(&opts.config);
        let mut file = File::open(&path)
            .map_err(|e| format!("could not open file: {}", e))
            .unwrap();
        let mut xml = String::new();
        file.read_to_string(&mut xml)
            .map_err(|e| format!("could not read: {}", e))
            .unwrap();
        opts.ebus_xml = Vec::from(xml.as_bytes());
    }

    opts.subcmd.run(&opts);
}

fn dump(opts: &Opts) {
    let conf = model::read_config(&opts.ebus_xml).unwrap();
    for dev in conf.devices.devices {
        println!("device: address={} name={}", dev.address, dev.name);
        for d in dev.descriptions {
            println!("        {}: {}", d.lang, d.text)
        }
    }
    for p in conf.packets.packets {
        println!(
            "packet: primary={:02} secondary={:02} name={}",
            p.primary, p.secondary, p.name
        );
        for d in p.descriptions {
            println!("        {}: {}", d.lang, d.text);
        }
        for f in p.fields.fields.unwrap() {
            match f {
                model::PacketField::Byte { offset, name } => {
                    println!("        [{:02}] Byte: {}", offset, name);
                }
                model::PacketField::Data1b { offset, name } => {
                    println!("        [{:02}] Data1b: {}", offset, name)
                }
                model::PacketField::Data1c { offset, name } => {
                    println!("        [{:02}] Data1c: {}", offset, name)
                }
                model::PacketField::Data2b { offset, name } => {
                    println!("        [{:02}] Data2b: {}", offset, name)
                }
                model::PacketField::Data2c { offset, name } => {
                    println!("        [{:02}] Data1c: {}", offset, name)
                }
                model::PacketField::ByteEnum {
                    offset,
                    name,
                    options,
                } => {
                    println!("        [{:02}] ByteEnum: {}", offset, name);
                    for o in options.0 {
                        println!("             [{:02}] {}", o.value, o.name);
                    }
                }
                model::PacketField::Bcd { offset, name } => {
                    println!("        [{:02}] Bcd: {}", offset, name)
                }
                model::PacketField::Bit { offset, name } => {
                    println!("        [{:02}] Bit: {}", offset, name)
                }
                model::PacketField::Word { offset, name } => {
                    println!("        [{:02}] Word: {}", offset, name)
                }
                model::PacketField::String {
                    offset,
                    name,
                    length,
                } => {
                    println!("        [{:02}] String: {} / {}", offset, name, length)
                }
            }
        }
    }
}

fn dump_l2(opts: &Opts) {
    let mut buf = BytesMut::with_capacity(1024);
    let mut other: u8 = 0;
    let mut sync = false;

    for b in io::stdin().bytes() {
        let mut b = b.unwrap();
        if opts.enhanced && b >= 0x80 {
            if other == 0 {
                if b & 0xc0 == 0xc0 {
                    other = b;
                }
                continue;
            } else {
                //  byte-1   byte-2
                // 76543210 76543210
                // 11ccccdd 10dddddd
                // c: command (0x01 = receive)
                // d: data (here b)
                b = (other & 0x03) << 6 | b & 0x3f;
                other = 0;
            }
        }
        if !sync {
            if b == 0xaa {
                sync = true;
            }
            continue;
        }
        buf.put_u8(b);

        if b == 0xaa {
            if buf.len() > 1 {
                println!(
                    "{:#02x?}",
                    buf.iter()
                        .map(|v| format!("0x{:02x}", v))
                        .collect::<Vec<String>>()
                        .join(", ")
                );
            }
            buf.clear()
        }
    }
}

fn parse_l2(opts: &Opts, cb: &dyn Fn(layer2::Packet)) {
    let mut buf = BytesMut::with_capacity(1024);
    let mut other: u8 = 0;
    let mut sync = false;
    for b in io::stdin().bytes() {
        let mut b = b.unwrap();
        if opts.enhanced && b >= 0x80 {
            if other == 0 {
                if b & 0xc0 == 0xc0 {
                    other = b;
                }
                continue;
            } else {
                //  byte-1   byte-2
                // 76543210 76543210
                // 11ccccdd 10dddddd
                // c: command (0x01 = receive)
                // d: data (here b)
                b = (other & 0x03) << 6 | b & 0x3f;
                other = 0;
            }
        }
        if !sync {
            if b == 0xaa {
                sync = true;
            }
            continue;
        }
        if b == 0xaa {
            if buf.len() > 1 {
                buf.put_u8(0xaa);
                let l2p = layer2::parse(&buf);
                match l2p.ok() {
                    Some(p) => {
                        if p.calc_crc() == p.crc {
                            log::debug!(
                                "{:#02x?} from {:#02x?}",
                                p,
                                buf.iter()
                                    .map(|v| format!("0x{:02x}", v))
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            );
                        } else {
                            log::info!(
                                "CRC-Fail: [{}] should be crc={:#02x}",
                                buf.iter()
                                    .map(|v| format!("0x{:02x}", v))
                                    .collect::<Vec<String>>()
                                    .join(", "),
                                p.calc_crc()
                            );
                        }
                        cb(p);
                    }
                    None => {
                        if buf.len() > 2 {
                            log::info!(
                                "Discard:  [{}]",
                                buf.iter()
                                    .map(|v| format!("0x{:02x}", v))
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            );
                        }
                    }
                }
                buf.clear();
            }
            continue;
        }
        buf.put_u8(b);
    }
}

fn parse_l7(opts: &Opts, cb: &dyn Fn(String, String, String, &Vec<layer7::DecodedField>)) {
    let conf = model::read_config(&opts.ebus_xml).unwrap();

    parse_l2(opts, &|p| {
        let pack = conf.packets.get(p.primary, p.secondary);
        if pack.is_none() {
            log::info!("No definition: {:?}", p);
            return;
        }
        let pack = pack.unwrap();
        let values = pack
            .fields
            .fields
            .as_ref()
            .map(|fields| layer7::decode_fields(&p, fields));

        let source = conf
            .devices
            .devices
            .iter()
            .find(|d| d.address == p.source)
            .map(|d| d.name.clone())
            .unwrap_or_else(|| format!("{:#02x}", p.source));
        let destination = conf
            .devices
            .devices
            .iter()
            .find(|d| d.address == p.destination)
            .map(|d| d.name.clone())
            .unwrap_or_else(|| format!("{:#02x}", p.destination));

        log::info!(
            "{} -> {}: {} {}",
            source,
            destination,
            pack.name,
            values
                .as_ref()
                .unwrap_or(&Vec::new())
                .iter()
                .map(|field| format!(
                    "{}={}",
                    field.name.clone(),
                    field
                        .v
                        .as_ref()
                        .map(|v2| v2.convert())
                        .unwrap_or_else(|| String::from("<>"))
                ))
                .collect::<Vec<String>>()
                .join(", ")
        );
        if let Some(values) = values {
            cb(source, destination, pack.name.clone(), &values);
        }
    })
}

impl From<&layer7::types::Value> for rinfluxdb::line_protocol::FieldValue {
    fn from(v: &layer7::types::Value) -> Self {
        match v {
            layer7::types::Value::Bool(v) => rinfluxdb::line_protocol::FieldValue::from(*v),
            layer7::types::Value::I8(v) => rinfluxdb::line_protocol::FieldValue::from(*v as i64),
            layer7::types::Value::U8(v) => rinfluxdb::line_protocol::FieldValue::from(*v as u64),
            layer7::types::Value::U16(v) => rinfluxdb::line_protocol::FieldValue::from(*v as u64),
            layer7::types::Value::F32(v) => rinfluxdb::line_protocol::FieldValue::from(*v as f64),
            layer7::types::Value::String(v) => {
                rinfluxdb::line_protocol::FieldValue::from(v.clone())
            }
        }
    }
}

fn influxdb(opts: &Opts, cfg: &Influxdb) {
    use rinfluxdb::line_protocol;
    use rinfluxdb::line_protocol::blocking::Client;
    use rinfluxdb::line_protocol::LineBuilder;
    use url::Url;

    let url = Url::parse(&cfg.url).unwrap();
    let client = Client::new(url, Some(("", ""))).unwrap();

    parse_l7(opts, &|source, destination, name, values| {
        let lines = values
            .iter()
            .filter(|df| df.v.is_some()) // filter ersatzwert values
            .map(|df| {
                let l = LineBuilder::new(cfg.measurement.clone())
                    .insert_tag("source", source.clone())
                    .insert_tag("destination", destination.clone())
                    .insert_field(
                        format!("{}.{}", name.clone(), df.name.clone()),
                        df.v.as_ref().unwrap(),
                    )
                    .build();
                log::debug!("{:?}", l);
                l
            })
            .collect::<Vec<line_protocol::Line>>();

        match client.send(&cfg.db, &lines) {
            Ok(_) => (),
            Err(err) => {
                log::error!("Sleep 1s after error sending values to influxdb: {}", err);
                thread::sleep(time::Duration::from_millis(1000));
            }
        };
    });
}
