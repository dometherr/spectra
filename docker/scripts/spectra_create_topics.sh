#!/usr/bin/env bash
set -e

echo "Waiting for Kafka to become ready on spectra-broker:9092..."
while ! nc -z spectra-broker 19092; do
  sleep 1
done

echo "Creating spectra.engine.v1.lifecycle topic..."
/opt/kafka/bin/kafka-topics.sh --bootstrap-server spectra-broker:19092 --create --if-not-exists \
  --topic spectra.engine.v1.lifecycle \
  --partitions 3 \
  --replication-factor 1

echo "Creating spectra.signal.v1.created topic..."
/opt/kafka/bin/kafka-topics.sh --bootstrap-server spectra-broker:19092 --create --if-not-exists \
  --topic spectra.signal.v1.created \
  --partitions 3 \
  --replication-factor 1

echo "Creating spectra.agent.v1.signal topic..."
/opt/kafka/bin/kafka-topics.sh --bootstrap-server spectra-broker:19092 --create --if-not-exists \
  --topic spectra.agent.v1.signal \
  --partitions 3 \
  --replication-factor 1

echo "Create topics has finished"
