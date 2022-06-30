include "../src//Collections/Sequences/Uint8_32.dfy"
module Uint8_32UnitTests {
import Uint8_32
import Uint8_32.Uint8Seq
import Uint8_32.Uint32Seq
import DivMod
import DivInternalsNonlinear
import DivInternals
import GeneralInternals
import ModInternals
import MulInternals
import MulInternalsNonlinear
import Mul
import ModInternalsNonlinear
import Power
import Seq
import Wrappers
import Math
method {:test} test0() {
var r0 := Uint8_32.ToLarge([]);
}
/*method {:test} test1() {
var r0 := Uint8_32.ToLarge([0, 0, 0, 0, 0, 0, 0]);
}*/
method {:test} test2() {
var r0 := Uint8_32.ToSmall([]);
}
method {:test} test3() {
var r0 := Uint8_32.Uint32Seq.SeqSub([0, 0, 0, 0, 0, 0, 0, 2475], [0, 0, 0, 0, 0, 0, 0, 1888]);
}
method {:test} test4() {
var r0 := Uint8_32.Uint32Seq.SeqSub([0, 0, 535], [0, 0, 536]);
}
method {:test} test5() {
var r0 := Uint8_32.Uint32Seq.SeqSub([], []);
}
method {:test} test6() {
var r0 := Uint8_32.Uint32Seq.SeqAdd([0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0]);
}
method {:test} test7() {
var r0 := Uint8_32.Uint32Seq.SeqAdd([0, 0, 0, 0, 8098], [0, 0, 0, 0, 16953]);
}
method {:test} test8() {
var r0 := Uint8_32.Uint32Seq.SeqAdd([], []);
}
method {:test} test9() {
var r0 := Uint8_32.Uint32Seq.SeqZero((1 as nat));
}
method {:test} test10() {
var r0 := Uint8_32.Uint32Seq.FromNatWithLen((1236 as nat), (1 as nat));
}
method {:test} test11() {
var r0 := Uint8_32.Uint32Seq.SeqExtendMultiple([0, 0], (3 as nat));
}
method {:test} test12() {
var r0 := Uint8_32.Uint32Seq.SeqExtend([0], (1 as nat));
}
method {:test} test13() {
var r0 := Uint8_32.Uint32Seq.SeqExtend([0, 0, 0, 0, 0, 0], (7 as nat));
}
method {:test} test14() {
var r0 := Uint8_32.Uint32Seq.FromNat((2279 as nat));
}
method {:test} test15() {
var r0 := Uint8_32.Uint32Seq.FromNat((0 as nat));
}
method {:test} test16() {
var r0 := Uint8_32.Uint32Seq.ToNatLeft([]);
}
method {:test} test17() {
var r0 := Uint8_32.Uint32Seq.ToNatRight([]);
}
method {:test} test18() {
var r0 := Uint8_32.Uint8Seq.SeqSub([0, 4044], [0, 1653]);
}
method {:test} test19() {
var r0 := Uint8_32.Uint8Seq.SeqSub([0, 0, 535], [0, 0, 536]);
}
method {:test} test20() {
var r0 := Uint8_32.Uint8Seq.SeqSub([], []);
}
method {:test} test21() {
var r0 := Uint8_32.Uint8Seq.SeqAdd([0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0]);
}
method {:test} test22() {
var r0 := Uint8_32.Uint8Seq.SeqAdd([0, 0, 0, 0, 8098], [0, 0, 0, 0, 16953]);
}
method {:test} test23() {
var r0 := Uint8_32.Uint8Seq.SeqAdd([], []);
}
method {:test} test24() {
var r0 := Uint8_32.Uint8Seq.SeqZero((1 as nat));
}
method {:test} test25() {
var r0 := Uint8_32.Uint8Seq.FromNatWithLen((1236 as nat), (1 as nat));
}
method {:test} test26() {
var r0 := Uint8_32.Uint8Seq.SeqExtendMultiple([0, 0], (3 as nat));
}
method {:test} test27() {
var r0 := Uint8_32.Uint8Seq.SeqExtend([0], (1 as nat));
}
method {:test} test28() {
var r0 := Uint8_32.Uint8Seq.SeqExtend([0, 0, 0, 0, 0, 0], (7 as nat));
}
method {:test} test29() {
var r0 := Uint8_32.Uint8Seq.FromNat((2279 as nat));
}
method {:test} test30() {
var r0 := Uint8_32.Uint8Seq.FromNat((0 as nat));
}
method {:test} test31() {
var r0 := Uint8_32.Uint8Seq.ToNatLeft([]);
}
method {:test} test32() {
var r0 := Uint8_32.Uint8Seq.ToNatRight([]);
}

}
