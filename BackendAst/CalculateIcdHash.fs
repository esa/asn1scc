module CalculateIcdHash

open System
open System.IO
open System.Numerics
open System.Security.Cryptography
open DAst

// Helper functions for serialization
let writeOption (writer: BinaryWriter) (writeValue: 'a -> unit) (opt: 'a option) =
    match opt with
    | Some value ->
        writer.Write(true)
        writeValue value
    | None ->
        writer.Write(false)

let writeList (writer: BinaryWriter) (writeItem: 'a -> unit) (lst: 'a list) =
    writer.Write(lst.Length)
    lst |> List.iter writeItem

let writeString (writer: BinaryWriter) (str: string) =
    writer.Write(str)

let writeInt (writer: BinaryWriter) (i : int) =
    writer.Write(i)

let writeBigInteger (writer: BinaryWriter) (bi: BigInteger) =
    let bytes = bi.ToByteArray()
    writer.Write(bytes.Length)
    writer.Write(bytes)

let writeIcdTypeCol (writer: BinaryWriter) (icdTypeCol: IcdTypeCol) =
    match icdTypeCol with
    | TypeHash s ->
        writer.Write(0uy) // Tag for TypeHash
        writeString writer s
    | IcdPlainType s ->
        writer.Write(1uy) // Tag for IcdPlainType
        writeString writer s

let writeIcdRowType (writer: BinaryWriter) (icdRowType: IcdRowType) =
    let tag =
        match icdRowType with
        | FieldRow -> 0uy
        | ReferenceToCompositeTypeRow -> 1uy
        | LengthDeterminantRow -> 2uy
        | PresentDeterminantRow -> 3uy
        | ThreeDOTs -> 4uy
    writer.Write(tag)

let writeIcdRow (w: BinaryWriter) (icdRow: IcdRow) =
    writeOption w (writeInt w) icdRow.idxOffset
    writeString w icdRow.fieldName
    writeList w (writeString w) icdRow.comments
    writeString w icdRow.sPresent
    writeIcdTypeCol w icdRow.sType
    writeOption w (writeString w) icdRow.sConstraint
    writeBigInteger w icdRow.minLengthInBits
    writeBigInteger w icdRow.maxLengthInBits
    writeOption w (writeString w) icdRow.sUnits
    writeIcdRowType w icdRow.rowType

let calcIcdTypeAssHash (t1: IcdTypeAss) =
    use ms = new MemoryStream()
    use w = new BinaryWriter(ms)

    // Serialize the IcdTypeAss fields
    writeOption w (writeString w) t1.asn1Link
    writeOption w (writeString w) t1.acnLink
    writeString w t1.name
    writeString w t1.kind
    writeList w (writeString w) t1.comments
    writeBigInteger w t1.minLengthInBytes
    writeBigInteger w t1.maxLengthInBytes
    writeList w (writeIcdRow w) t1.rows

    w.Flush()
    ms.Position <- 0L
    let bytes = ms.ToArray()
    // Compute the hash
    let hashAlgorithm = MD5.Create()
    let hash = hashAlgorithm.ComputeHash(bytes)
    Convert.ToHexString(hash)


