#!/bin/bash
export DOTNET_ROOT=$HOME/.dotnet
export PATH=$DOTNET_ROOT:$DOTNET_ROOT/tools:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ASN1SCC="dotnet /home/maxime/workspace/asn1scc/asn1scc/bin/Release/net10.0/asn1scc.dll"
RUST_RUNTIME=/home/maxime/workspace/asn1scc/asn1rust
TEST_BASE=/home/maxime/workspace/asn1scc/v4Tests/test-cases/acn
OUT_BASE=/home/maxime/workspace/asn1scc/v4Tests/tmp_rust_diag

run_test() {
    local test_name="$1"
    local asn1_file="$2"
    local acn_file="$3"
    local out_dir="$OUT_BASE/$test_name"
    
    echo "============================================"
    echo "TEST: $test_name"
    echo "ASN1: $asn1_file"
    echo "ACN:  $acn_file"
    echo "============================================"
    
    rm -rf "$out_dir"
    mkdir -p "$out_dir"
    cp "$asn1_file" "$out_dir/sample1.asn1"
    if [ -n "$acn_file" ] && [ -f "$acn_file" ]; then
        cp "$acn_file" "$out_dir/sample1.acn"
    else
        echo "TEST-CASE DEFINITIONS ::= BEGIN" > "$out_dir/sample1.acn"
        echo "END" >> "$out_dir/sample1.acn"
    fi
    
    cd "$out_dir"
    $ASN1SCC -Rust -uPER -ACN -equal -atc -o "$out_dir" sample1.asn1 sample1.acn 2>tmp.err
    local gen_exit=$?
    echo "Code gen exit: $gen_exit"
    if [ $gen_exit -ne 0 ]; then
        echo "--- ERROR (code generation failed) ---"
        cat tmp.err | head -30
        echo ""
        return
    fi
    
    # Copy runtime
    rm -rf "$out_dir/asn1rust"
    cp -r "$RUST_RUNTIME" "$out_dir/asn1rust"
    rm -rf "$out_dir/asn1rust/target" "$out_dir/asn1rust/Cargo.lock"
    
    # Create Cargo.toml
    cat > "$out_dir/Cargo.toml" << 'CARGO'
[package]
name = "asn1scc_test"
version = "0.1.0"
edition = "2021"

[dependencies]
asn1rust = { path = "asn1rust" }

[[bin]]
name = "mainprogram"
path = "mainprogram.rs"
CARGO
    
    cargo build 2>covlog.txt
    local build_exit=$?
    echo "Cargo build exit: $build_exit"
    if [ $build_exit -ne 0 ]; then
        echo "--- ERROR (compilation failed) ---"
        head -50 covlog.txt
        echo ""
        return
    fi
    
    cargo run 2>>covlog.txt
    local run_exit=$?
    echo "Cargo run exit: $run_exit"
    if [ $run_exit -ne 0 ]; then
        echo "--- ERROR (run failed) ---"
        tail -20 covlog.txt
    fi
    echo ""
}

# Test 1: 01-INTEGER/005
run_test "01-INTEGER-005" "$TEST_BASE/01-INTEGER/005.asn1" ""

# Test 2: 16-mantis/0000774
run_test "16-mantis-0000774" "$TEST_BASE/16-mantis/0000774-DataTypesSimulink.asn1" ""

# Test 3: 16-mantis/0000806
run_test "16-mantis-0000806" "$TEST_BASE/16-mantis/0000806.asn1" ""

# Test 4: 16-mantis/0000807
run_test "16-mantis-0000807" "$TEST_BASE/16-mantis/0000807.asn1" "$TEST_BASE/16-mantis/0000807.acn"

# Test 5: 16-mantis/0000807b
run_test "16-mantis-0000807b" "$TEST_BASE/16-mantis/0000807b.asn1" "$TEST_BASE/16-mantis/0000807b.acn"

# Test 6: 16-mantis/000724
run_test "16-mantis-000724" "$TEST_BASE/16-mantis/000724-DataTypesSimulink.asn1" ""

# Test 7: 20-WithComponents/008
run_test "20-WithComponents-008" "$TEST_BASE/20-WithComponents/008.asn1" ""
