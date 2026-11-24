(defsystem "harness-demos-mcp-host"
  :version "0.1.0"
  :description "Demo of mcp-protocol 0.5.0 host answers against a stub MCP server over rpc-backend-stdio's in-memory pipe pair: client capabilities from its handlers (a 2026-07-28 request's _meta, a 2025-11-25 initialize, explicit capabilities replacing them whole), sampling and elicitation answered over MRTR rounds, a handler's refusal, the round limit and a client without a sampling handler"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "mcp-protocol" "0.5.0")
               (:version "rpc-backend-stdio" "0.3.2")
               "rpc-protocol"
               "rpc-protocol-json"
               "json-protocol"
               "json-backend-jzon"
               "bordeaux-threads")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
