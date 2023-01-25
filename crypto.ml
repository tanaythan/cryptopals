(* https://discuss.ocaml.org/t/convert-hexadecimal-to-decimal/396 *)
let int_of_hex c =
  List.assoc c
    [
      ('0', 0);
      ('1', 1);
      ('2', 2);
      ('3', 3);
      ('4', 4);
      ('5', 5);
      ('6', 6);
      ('7', 7);
      ('8', 8);
      ('9', 9);
      ('a', 10);
      ('b', 11);
      ('c', 12);
      ('d', 13);
      ('e', 14);
      ('f', 15);
    ]

let hexToBase64 s =
  let len = String.length s in
  let paddingNum = len * 2 mod 3 in
  let paddedStr = if paddingNum > 0 then s ^ "0" else s in
  let rec convert byteStr i take =
    if Bytes.length byteStr <= i + 1 then []
    else
      let byte = int_of_hex (Bytes.get byteStr i) in
      let nextByte = int_of_hex (Bytes.get byteStr (i + 1)) in
      match take with
      | true ->
          ((byte lsl 2) lor ((nextByte land 0b1100) lsr 2))
          :: convert byteStr (i + 1) false
      | false ->
          (((byte land 0b0011) lsl 4) lor nextByte)
          :: convert byteStr (i + 2) true
  in
  let intToString c =
    if 0 <= c && c < 26 then Char.chr (c + 65)
    else if 26 <= c && c < 52 then Char.chr (c + 71)
    else if 52 <= c && c < 62 then Char.chr (c - 4)
    else if c == 62 then '+'
    else if c == 63 then '/'
    else assert false
  in

  String.of_seq
    (List.to_seq
       (List.map intToString (convert (Bytes.of_string paddedStr) 0 true)))
  ^ String.make paddingNum '='

let xorCombo buf1 buf2 =
  let rec xor i =
    if Bytes.length buf1 <= i then []
    else
      let buf1Char = int_of_hex (Bytes.get buf1 i) in
      let buf2Char = int_of_hex (Bytes.get buf2 i) in
      Printf.sprintf "%1x" (buf1Char lxor buf2Char) :: xor (i + 1)
  in
  String.concat "" (xor 0)

let main =
  let base64Given = hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" in
  let xorGiven = (xorCombo
       (String.to_bytes "1c0111001f010100061a024b53535009181c")
       (String.to_bytes "686974207468652062756c6c277320657965")) in
  Format.printf "1.1 %b\n" (String.equal base64Given "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t");
  Format.printf "1.2 %b\n" (String.equal xorGiven "746865206b696420646f6e277420706c6179");
