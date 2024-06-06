/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

pub fn print_result: Forall r: Rgn. (i32, handle(r))->0 =
    0 get /* dup handle */
    <u8[]@r> 2 malloc /* create output array */
    2 get /* get number */
    10 modulo /* get smallest digit */
    48 add /* convert to ASCII digit */
    i32_to_u8 /* convert to u8 */
    0 arr_mut /* put char in array */
    10u8 1 arr_mut /* put newline */
    <((()@r)->0, ()@r)> malloc
    2 get /* get handle */
    <()@r> malloc 1 init
    $print_handler 0 init
    <Exists a: 16byte. ((a)->0, a)> <()@r> pack
    0u8 /* write mode: stdout */
    3 get /* get handle */
    write /* print the one-character array */
    0u8 halt;

fn print_handler: ()->0 = 0u8 halt;