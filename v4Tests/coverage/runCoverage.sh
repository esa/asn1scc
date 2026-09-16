#!/usr/bin/env bash
# Host entry point: Bash and Docker only; all measurement tools run in images.
set -euo pipefail

metric=gcov
language=c
image=
outdir=
container=
encode_pilot=false
decode_pilot=false
invalid_value_pilot=false
invalid_stream_pilot=false

usage() {
    cat <<'EOF'
Usage: runCoverage.sh [--metric gcov|stmt] [--language c|Ada]
                      [--image IMAGE] [--outdir DIRECTORY] [--name CONTAINER]
                      [--encode-pilot|--decode-pilot|--invalid-value-pilot|--invalid-stream-pilot]
                      [-- COLLECTOR_ARGUMENTS...]

Defaults to the bounded pilot cohort. Pass -- --cohort all for a full run.
--encode-pilot runs the paired three-fixture C experiment instead.
--decode-pilot measures baseline/actual-length/capped-prefix C stages instead.
--invalid-value-pilot compares positives with explicit invalid C value checks.
--invalid-stream-pilot compares positives with fixed-layout ACN stream mutations.
Images must already be built; measurement runs have no network or host mounts.
Containers and exported results are retained even when a measurement fails.
EOF
}

while (($#)); do
    case "$1" in
        --metric|--language|--image|--outdir|--name)
            if (($# < 2)); then usage >&2; exit 2; fi
            case "$1" in
                --metric) metric=$2 ;;
                --language) language=$2 ;;
                --image) image=$2 ;;
                --outdir) outdir=$2 ;;
                --name) container=$2 ;;
            esac
            shift 2 ;;
        --encode-pilot) encode_pilot=true; shift ;;
        --decode-pilot) decode_pilot=true; shift ;;
        --invalid-value-pilot) invalid_value_pilot=true; shift ;;
        --invalid-stream-pilot) invalid_stream_pilot=true; shift ;;
        --help|-h) usage; exit 0 ;;
        --) shift; break ;;
        *) usage >&2; exit 2 ;;
    esac
done
case "$language" in c|C) language=c ;; ada|Ada) language=Ada ;; *) usage >&2; exit 2 ;; esac
case "$metric" in gcov|stmt) ;; *) usage >&2; exit 2 ;; esac
for argument in "$@"; do
    case "$argument" in
        --language|--language=*|--outdir|--outdir=*)
            printf '%s\n' 'Set language/outdir as runner options before --.' >&2
            exit 2 ;;
    esac
done

if [[ "$metric" == gcov ]]; then
    image=${image:-asn1scc-coverage:local}
    script=/opt/coverage/coverageCollector.py
else
    case "$language" in
        c) image=${image:-asn1scc-coverage:statement-c} ;;
        Ada) image=${image:-asn1scc-coverage:statement-ada} ;;
    esac
    script=/opt/coverage/statementCollector.py
fi
arguments=(--outdir /results/measurement --language "$language" --cohort pilot)
if [[ "$invalid_stream_pilot" == true && ( "$encode_pilot" == true || "$decode_pilot" == true || "$invalid_value_pilot" == true || "$language" != c ) ]]; then
    printf '%s\n' '--invalid-stream-pilot requires C and cannot be combined with another pilot.' >&2
    exit 2
fi
if [[ "$invalid_value_pilot" == true && ( "$encode_pilot" == true || "$decode_pilot" == true || "$language" != c ) ]]; then
    printf '%s\n' '--invalid-value-pilot requires C and cannot be combined with another pilot.' >&2
    exit 2
fi
if [[ "$decode_pilot" == true && ( "$encode_pilot" == true || "$language" != c ) ]]; then
    printf '%s\n' '--decode-pilot requires C and cannot be combined with --encode-pilot.' >&2
    exit 2
fi
if [[ "$encode_pilot" == true ]]; then
    if [[ "$language" != c ]]; then
        printf '%s\n' 'The checked/unchecked encode pilot currently supports C only.' >&2
        exit 2
    fi
    case "$metric" in
        gcov) script=/opt/coverage/encodePilot.py ;;
        stmt) script=/opt/coverage/statementPilot.py ;;
    esac
    arguments=(--outdir /results/pilot)
fi
if [[ "$decode_pilot" == true ]]; then
    script=/opt/coverage/decodePilot.py
    arguments=(--outdir /results/pilot --metric "$metric")
fi
if [[ "$invalid_value_pilot" == true ]]; then
    script=/opt/coverage/invalidValuePilot.py
    arguments=(--outdir /results/pilot --metric "$metric")
fi
if [[ "$invalid_stream_pilot" == true ]]; then
    script=/opt/coverage/invalidStreamPilot.py
    arguments=(--outdir /results/pilot --metric "$metric")
fi

container=${container:-coverage-${metric}-${language,,}-$(date -u +%Y%m%dT%H%M%S)-${RANDOM}}
outdir=${outdir:-coverage-results/$container}
mkdir -p -- "$(dirname -- "$outdir")"
mkdir -- "$outdir"  # Refuse to overwrite an earlier result directory.
outdir=$(cd -- "$outdir" && pwd -P)
printf 'Image: %s\nContainer: %s\n' "$image" "$container"
docker create --pull=never --name "$container" --network none --user 10001:10001 \
    --entrypoint python3 "$image" "$script" "${arguments[@]}" "$@" > "$outdir/container.id"

status=0
docker start -a "$container" || status=$?
docker inspect --format '{"image":{{json .Image}},"user":{{json .Config.User}},"network":{{json .HostConfig.NetworkMode}},"mounts":{{json .Mounts}},"status":{{json .State.Status}},"exit_code":{{json .State.ExitCode}}}' \
    "$container" > "$outdir/container.json" || status=1
docker inspect --format '{{.State.Status}} {{.State.ExitCode}}' "$container" > "$outdir/container.status" || status=1
if read -r state code < "$outdir/container.status"; then
    if [[ "$state" != exited ]]; then status=1
    elif [[ "$code" != 0 ]]; then status=$code
    fi
else
    status=1
fi
docker cp "$container:/results/." "$outdir/results" || status=1
printf 'Results: %s\nExit status: %s\n' "$outdir" "$status"
exit "$status"
