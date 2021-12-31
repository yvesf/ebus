# ebus-rust

### Dumping sample to stdout
```shell
RUST_LOG=info cargo run parse-l7 < ../doc/sample_dump_2.bin
```

### Running against influxdb
Start influxdb:
```shell
influxdb run
```

At first time:
```shell
influx -execute 'create database ebus'
```
(default data directory is `$HOME/.influxdb`).

Insert dump:
```shell
RUST_LOG=info cargo run influxdb < ../doc/sample_dump_2.bin
```

Verify:
```sql
influx
> show databases
> use ebus
> show measurements
> SELECT * FROM ebus LIMIT 5;
```