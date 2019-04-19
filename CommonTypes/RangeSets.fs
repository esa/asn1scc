module RangeSets

type Range<'v when 'v : comparison> =
    | Range_NI_A of 'v*bool
    | Range_B_PI of 'v*bool
       

type RangeSet<'v when 'v : comparison> =
    | RangeSet of Range<'v> list


type RangeSet<'v when 'v : comparison> with
    member this.complement


