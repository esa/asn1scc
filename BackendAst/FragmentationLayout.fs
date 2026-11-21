/// Layout of the uPER fragmented encodings: the blocks of a fixed-size fragmented encoding
/// (DAstUPer.handleFixedSizeFragmentation) and the byte positions of their headers (coverage cycle 15d, used by the
/// decode sweep of the automatic tests, EncodeDecodeTestCase, which is compiled before DAstUPer).
module FragmentationLayout
open System.Numerics

let C64K = BigInteger 0x10000
let C48K = BigInteger 0xC000
let C32K = BigInteger 0x8000
let C16K = BigInteger 0x4000
let C_127 = BigInteger 0x7F

type FragmentationParts = {
    nBlocks64K :  BigInteger
    has48KBlock : bool
    has32KBlock : bool
    has16KBlock : bool
    nRemainingItemsVar : BigInteger
}

let FragmentationParts (size:BigInteger) =
    let nBlocks64K = size / C64K
    let nRemainingItemsVar1 = size % C64K
    let has48KBlock = nRemainingItemsVar1 >= C48K

    let nRemainingItemsVar2 = if has48KBlock then (nRemainingItemsVar1 - C48K) else nRemainingItemsVar1
    let has32KBlock = nRemainingItemsVar2 >= C32K

    let nRemainingItemsVar3 = if has32KBlock then (nRemainingItemsVar2 - C32K) else nRemainingItemsVar2
    let has16KBlock = nRemainingItemsVar3 >= C16K

    let nRemainingItemsVar = if has16KBlock then (nRemainingItemsVar3 - C16K) else nRemainingItemsVar3

    { nBlocks64K = nBlocks64K; has48KBlock = has48KBlock; has32KBlock = has32KBlock; has16KBlock = has16KBlock; nRemainingItemsVar = nRemainingItemsVar}

/// Coverage cycle 15d: the bits of a fixed-size fragmented encoding of size items of itemBits bits (headers included).
let fixedSizeFragmentedBits (size:BigInteger) (itemBits:BigInteger) : BigInteger =
    let r = FragmentationParts size
    let smallItems = [(r.has48KBlock, C48K); (r.has32KBlock, C32K); (r.has16KBlock, C16K)] |> List.filter fst |> List.sumBy snd
    let nSmallHeaders = [r.has48KBlock; r.has32KBlock; r.has16KBlock] |> List.filter id |> List.length |> BigInteger
    let restHeaderBits =
        match r.nRemainingItemsVar > 0I, r.nRemainingItemsVar <= C_127 with
        | false, _     -> 0I
        | true, true   -> 8I
        | true, false  -> 16I
    8I * r.nBlocks64K + 8I * nSmallHeaders + restHeaderBits + (r.nBlocks64K * C64K + smallItems + r.nRemainingItemsVar) * itemBits

/// Coverage cycle 15d: byte positions of the headers of a fixed-size fragmented encoding (handleFixedSizeFragmentation:
/// a 0xC4 octet per 64K block, one 0xC3 / 0xC2 / 0xC1 octet for a 48K / 32K / 16K block, then the length of the rest
/// in 1 octet (<= 127) or 2) that starts at bit 0, each with the byte after it (the first items of its block); the
/// decode sweep of the automatic tests corrupts them. Every block holds a multiple of 16K items, so every header is
/// byte aligned whatever the item size. Capped to the first and the last 64K header (the others run the same loop),
/// the small block header and the length of the rest: at most 9 positions for any size. Only positions inside the
/// encoding (items of 0 bits, FROM("A"): the byte after a header may not exist).
let fixedSizeFragmentationHeaderBytes (size:BigInteger) (itemBits:BigInteger) : BigInteger list =
    let r = FragmentationParts size
    let blockBytes (nItems:BigInteger) = 1I + nItems * itemBits / 8I       // header octet + items (nItems = k x 16K)
    let n64 = r.nBlocks64K
    let smallBlock =
        [(r.has48KBlock, C48K); (r.has32KBlock, C32K); (r.has16KBlock, C16K)] |> List.tryFind fst |> Option.map snd
    let smallStart = n64 * blockBytes C64K
    let restStart = smallStart + (smallBlock |> Option.map blockBytes |> Option.defaultValue 0I)
    let encodedBytes = (fixedSizeFragmentedBits size itemBits + 7I) / 8I
    let headers =                                                           // (first byte, bytes)
        [
            if n64 > 0I then yield (0I, 1I)
            if n64 > 1I then yield ((n64 - 1I) * blockBytes C64K, 1I)
            match smallBlock with
            | Some _ -> yield (smallStart, 1I)
            | None   -> ()
            if r.nRemainingItemsVar > 0I then yield (restStart, (if r.nRemainingItemsVar <= C_127 then 1I else 2I))
        ]
    headers |> List.collect (fun (first, nBytes) -> [first .. first + nBytes]) |> List.distinct |> List.sort |>
    List.filter (fun p -> p < encodedBytes)
