include "LittleEndianNatConversions.dfy"

/* Conversions between sequences of uint32 and uint64. */
module Uint32_64 refines LittleEndianNatConversions {

  module {:extern "Uint32__64_mUint32Seq_Compile"} Uint32Seq refines SmallSeq {
    function method BITS(): nat { 32 }
  }

  module {:extern "Uint32__64_mUint64Seq_Compile"} Uint64Seq refines LargeSeq {
    import Small = Uint32Seq
    function method BITS(): nat { 64 }
  }

  import opened Large = Uint64Seq
  import Small = Large.Small

}
