#!/usr/bin/env bash
set -e

echo "Waiting readiness for Spectra Broker@19092..."

while ! nc -z spectra-broker 19092; do
  sleep 1
done

echo "Spectra Broker is ready, proceeding with topics creation..."

function create_topic() {
  local topic="$1"
  local partitions="$2"
  local replication="$3"

  echo "Checking topic: ${topic} readiness..."
  /opt/kafka/bin/kafka-topics.sh \
    --bootstrap-server spectra-broker:19092 \
    --create \
    --if-not-exists \
    --topic "${topic}" \
    --partitions "${partitions}" \
    --replication-factor "${replication}"
}

create_topic "spectra.agent.v1.signal" 3 1
create_topic "spectra.engine.v1.lifecycle" 3 1
create_topic "spectra.signal.v1.created" 3 1
create_topic "spectra.symbol.v1.enriched" 3 1

echo "Spectra topics are ready"
