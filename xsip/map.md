12:41:39.989 [debug] server transaction ~K[z9hG4bK-524287-1---3ad785f88ab0ce91|:register|127.0.0.1:43730] started
%Sippet.Message{
  body: "",
  headers: %{
    :allow => ["INVITE", "ACK", "CANCEL", "BYE", "NOTIFY", "REFER", "MESSAGE",
     "OPTIONS", "INFO", "SUBSCRIBE"],
    :call_id => "CdcZk-5736NzkBA0Ioc2SA..",
    :contact => [
      {"",
       %Sippet.URI{
         authority: "1002@127.0.0.1:43730",
         headers: nil,
         host: "127.0.0.1",
         parameters: ";rinstance=5e3aae223f643f20;transport=UDP",
         port: 43730,
         scheme: "sip",
         userinfo: "1002"
       }, %{}}
    ],
    :content_length => 0,
    :cseq => {1, :register},
    :expires => 60,
    :from => {"",
     %Sippet.URI{
       authority: "1002@127.0.0.1",
       headers: nil,
       host: "127.0.0.1",
       parameters: ";transport=UDP",
       port: 5060,
       scheme: "sip",
       userinfo: "1002"
     }, %{"tag" => "c24afb47"}},
    :max_forwards => 70,
    :to => {"",
     %Sippet.URI{
       authority: "1002@127.0.0.1",
       headers: nil,
       host: "127.0.0.1",
       parameters: ";transport=UDP",
       port: 5060,
       scheme: "sip",
       userinfo: "1002"
     }, %{}},
    :user_agent => "Z 5.5.5 v2.10.15.1-40-g487dc44b5",
    :via => [
      {{2, 0}, :udp, {"127.0.0.1", 43730},
       %{"branch" => "z9hG4bK-524287-1---3ad785f88ab0ce91", "rport" => ""}}
    ],
    "Allow-Events" => ["presence, kpml, talk"]
  },
  start_line: %Sippet.Message.RequestLine{
    method: :register,
    request_uri: %Sippet.URI{
      authority: "127.0.0.1",
      headers: nil,
      host: "127.0.0.1",
      parameters: ";transport=UDP",
      port: 5060,
      scheme: "sip",
      userinfo: nil
    },
    version: {2, 0}
  },
  target: nil
}
