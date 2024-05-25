/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

pub fn print_result: Forall r: Rgn. (i32, handle(r))->0 =
    <u8[]@r> 2 malloc /* create output array */
    1 get /* get number */
    10 modulo /* get smallest digit */
    48 add /* convert to ASCII digit */
    i32_to_u8 /* convert to u8 */
    0 arr_mut /* put char in array */
    10u8 1 arr_mut /* put newline at end of string */
    print /* print one-character array */
    0u8 halt;