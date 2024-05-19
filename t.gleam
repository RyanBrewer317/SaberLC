fn main-1[r60](h61: handle(r60)) {
    let tpl43 = malloc(h61): ()@r60
    let tpl44 = malloc(h61): (forall r62: Rgn. (handle(r62), ()@r62, some a25. (forall r63: Rgn. (handle(r63), a25, i32, some a26. (forall r64: Rgn. (handle(r64), a26, i32)->0, a26)@r63)->0, a25)@r62)->0, ()@r60)@r60
    let tpl45 = tpl44[0] <- foo39
    let tpl46 = tpl45[1] <- tpl43
    let [a27, clos5] = unpack(pack tpl46 hiding ()@r60 as some a27. (forall r77: Rgn. (handle(r77), a27, some a28. (forall r78: Rgn. (handle(r78), a28, i32, some a29. (forall r79: Rgn. (handle(r79), a29, i32)->0, a29)@r78)->0, a28)@r77)->0, a27)@r60)
    let f6: forall r101: Rgn. (handle(r101), a27, some a28. (forall r102: Rgn. (handle(r102), a28, i32, some a29. (forall r103: Rgn. (handle(r103), a29, i32)->0, a29)@r102)->0, a28)@r101)->0 = f5[0]
    let env7: a27 = clos5[1]
    let tpl47 = malloc(h61): ()@r60
    let tpl48 = malloc(h61): (forall r86: Rgn. (handle(r86), ()@r86, i32, some a36. (forall r87: Rgn. (handle(r87), a36, i32)->0, a36)@r86)->0, ()@r60)@r60
    let tpl49 = tpl48[0] <- foo42
    let tpl50 = tpl49[1] <- tpl47
    f6[r60](h61, env7, pack tpl50 hiding ()@r60 as some a37. (forall r99: Rgn. (handle(r99), a37, i32, some a38. (forall r100: Rgn. (handle(r100), a38, i32)->0, a38)@r99)->0, a37)@r60)
}

fn foo39[r104](h105: handle(r104), env10: ()@r104, f2: some a25. (forall r106: Rgn. (handle(r106), a25, i32, some a26. (forall r107: Rgn. (handle(r107), a26, i32)->0, a26)@r106)->0, a25)@r104) {
    let tpl51 = malloc(h105): (some a14. (forall r108: Rgn. (handle(r108), a14, i32, some a15. (forall r109: Rgn. (handle(r109), a15, i32)->0, a15)@r108)->0, a14)@r104)@r104
    let tpl52 = tpl51[0] <- f2
    let tpl53 = malloc(h105): (forall r114: Rgn. (handle(r114), (some a14. (forall r115: Rgn. (handle(r115), a14, i32, some a15. (forall r116: Rgn. (handle(r116), a15, i32)->0, a15)@r115)->0, a14)@r114)@r114, i32)->0, (some a14. (forall r117: Rgn. (handle(r117), a14, i32, some a15. (forall r118: Rgn. (handle(r118), a15, i32)->0, a15)@r117)->0, a14)@r104)@r104)@r104
    let tpl54 = tpl53[0] <- foo40
    let tpl55 = tpl54[1] <- tpl52
    let [a24, clos11] = unpack(pack tpl55 hiding (some a14. (forall r134: Rgn. (handle(r134), a14, i32, some a15. (forall r135: Rgn. (handle(r135), a15, i32)->0, a15)@r134)->0, a14)@r104)@r104 as some a24. (forall r141: Rgn. (handle(r141), a24, i32)->0, a24)@r104)
    let f12: forall r145: Rgn. (handle(r145), a24, i32)->0 = f11[0]
    let env13: a24 = clos11[1]
    f12[r104](h105, env13, 3)
}

fn foo40[r146](h147: handle(r146), env16: (some a14. (forall r148: Rgn. (handle(r148), a14, i32, some a15. (forall r149: Rgn. (handle(r149), a15, i32)->0, a15)@r148)->0, a14)@r146)@r146, x3: i32) {
    let f2: some a14. (forall r168: Rgn. (handle(r168), a14, i32, some a15. (forall r169: Rgn. (handle(r169), a15, i32)->0, a15)@r168)->0, a14)@r146 = f16[0]
    let [a20, clos17] = unpack(f2)
    let f18: forall r166: Rgn. (handle(r166), a20, i32, some a21. (forall r167: Rgn. (handle(r167), a21, i32)->0, a21)@r166)->0 = f17[0]
    let env19: a20 = clos17[1]
    let tpl56 = malloc(h147): ()@r146
    let tpl57 = malloc(h147): (forall r158: Rgn. (handle(r158), ()@r158, i32)->0, ()@r146)@r146
    let tpl58 = tpl57[0] <- foo41
    let tpl59 = tpl58[1] <- tpl56
    f18[r146](h147, env19, x3, pack tpl59 hiding ()@r146 as some a23. (forall r165: Rgn. (handle(r165), a23, i32)->0, a23)@r146)
}

fn foo41[r170](h171: handle(r170), env22: ()@r170, x1: i32) {
    halt x1
}

fn foo42[r172](h173: handle(r172), env31: ()@r172, x0: i32, k4: some a36. (forall r174: Rgn. (handle(r174), a36, i32)->0, a36)@r172) {
    let [a35, clos32] = unpack(k4)
    let f33: forall r179: Rgn. (handle(r179), a35, i32)->0 = f32[0]
    let env34: a35 = clos32[1]
    f33[r172](h173, env34, x0)
}