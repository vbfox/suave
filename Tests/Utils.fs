﻿module Suave.Tests.Utils

open System

open Suave
open Suave.Utils.Compression
open Suave.Utils.Parsing

open Fuchu

open System.Text

[<Tests>]
let utilities =
  testList "trying some utility functions" [
    testCase "loopback ipv4" <| fun _ ->
      Assert.Equal("127.0.0.1 is a local address", true, is_local_address "127.0.0.1")

    testCase "loopback ipv6" <| fun _ ->
      Assert.Equal("::0 is a local address", true, is_local_address "::1")

    testProperty "gzip_encode/gzip_decode" <| fun str ->
      let get_bytes  = Encoding.UTF8.GetBytes  : string -> byte []
      let from_bytes = Encoding.UTF8.GetString : byte [] -> string
      Assert.Equal(
        "compress >> decompress == identity",
        str, (get_bytes >> gzip_encode >> gzip_decode >> from_bytes) str)

    testCase "str eql" <| fun _ ->
      for (str1, str2) in
        [
          "", ""
          "a", "b"
          "a", "aa"
          "aa", "aa"
        ] do
        Assert.Equal("should have same as normal str equal",
                     str1.Equals str2,
                     String.cnst_time_cmp_ord str1 str2)

    testCase "crypto hello world" <| fun _ ->
      let str = "Hello World"
      let ca (str : string) = str.ToCharArray() |> Array.map (string<<int) |> String.concat ","
      let key = Crypto.generate_key' ()
      let (Choice1Of2 cipher) = Crypto.secretbox key str
      let (Choice1Of2 plain) = Crypto.secretbox_open key cipher
      Assert.Equal(sprintf "'%s':%s = D(k, E(k, '%s':%s))" plain (ca plain) str (ca str), str, plain)

    testProperty "can encrypt and then decrypt any string" <| fun (str : string) ->
      let ca (str : string) = str.ToCharArray() |> Array.map (string<<int) |> String.concat ","
      if not <| String.IsNullOrWhiteSpace str then
        let key = Crypto.generate_key' ()
        let (Choice1Of2 cipher) = Crypto.secretbox key str
        let (Choice1Of2 plain) = Crypto.secretbox_open key cipher
        Assert.Equal(sprintf "'%s':%s = D(k, E(k, '%s':%s))" plain (ca plain) str (ca str), str, plain)
  ]
