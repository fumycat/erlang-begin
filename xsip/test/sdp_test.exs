defmodule SdpTest do
  use ExUnit.Case
  doctest Sdp

  test "parse sdp empty" do
    assert Sdp.parse_sdp_body("") == []
  end

  test "parse sdp something" do
    assert Sdp.parse_sdp_body("v=0\r\nm=audio 12345 bla bla\r\nt=0 0") == [
             v: "0",
             m: "audio 12345 bla bla",
             t: "0 0"
           ]
  end

  test "parse sdp wrong order" do
    refute Sdp.parse_sdp_body("v=0\r\nt= 0 0") == [t: "0 0", v: "0"]
  end
end
