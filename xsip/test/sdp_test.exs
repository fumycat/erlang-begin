defmodule SdpTest do
  use ExUnit.Case
  doctest Sdp

  test "parse_sdp_body empty" do
    assert Sdp.parse_sdp_body("") == []
  end

  test "parse_sdp_body ok" do
    assert Sdp.parse_sdp_body("v=0\r\nm=audio 12345 bla bla\r\nt=0 0") == [
             v: "0",
             m: "audio 12345 bla bla",
             t: "0 0"
           ]
  end

  test "parse_sdp_body duplicates ok" do
    input = """
    v=0\r\n\
    o=Zoiper 1630120310317 1 IN IP4 192.168.0.104\r\n\
    s=Z\r\n\
    c=IN IP4 192.168.0.104\r\n\
    t=0 0\r\n\
    m=audio 52600 RTP/AVP 0 101 8 3\r\n\
    a=rtpmap:101 telephone-event/8000\r\n\
    a=fmtp:101 0-16\r\n\
    a=sendrecv\r\n\
    """

    expected = [
      v: "0",
      o: "Zoiper 1630120310317 1 IN IP4 192.168.0.104",
      s: "Z",
      c: "IN IP4 192.168.0.104",
      t: "0 0",
      m: "audio 52600 RTP/AVP 0 101 8 3",
      a: "rtpmap:101 telephone-event/8000",
      a: "fmtp:101 0-16",
      a: "sendrecv"
    ]

    assert Sdp.parse_sdp_body(input) == expected
  end

  test "parse_sdp_body order" do
    refute Sdp.parse_sdp_body("v=0\r\nt= 0 0") == [t: "0 0", v: "0"]
  end

  test "compose_sdp ok" do
    assert Sdp.compose_sdp([v: "0", a: "something", a: "another"]) == "v=0\r\na=something\r\na=another\r\n"
  end

  test "compose_sdp order" do
    refute Sdp.compose_sdp([v: "0", a: "something", a: "another"]) == "a=something\r\na=another\r\nv=0\r\n"
  end

  test "compose_sdp empty" do
    assert Sdp.compose_sdp([]) == ""
  end

end
