(defsystem "harness-demos-code-mcp-requests"
  :version "0.1.0"
  :description "Demo of cl-stack-code 0.16.0's H3e-2b MCP features against a stub stdio MCP server (mcp-protocol's server side on rpc-backend-stdio) and a scripted loopback OpenAI-compatible model: sampling denied headless, then allowed by an mcp_sampling(SERVER) rule with the server's maxTokens capped; elicitation declined headless, then accepted with content through the SDK's approval channel; deferred tool loading behind tool_search (mcpToolSearch always), a search loading a tool for the next step and a deferred tool called by its exact name"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cl-stack-code" "0.16.0")
               (:version "ai-agent-protocol" "0.17.0")
               (:version "mcp-protocol" "0.5.0")
               (:version "llm-protocol-openai" "0.8.0")
               (:version "http-backend-async" "0.2.28")
               "rpc-backend-stdio"
               "json-protocol"
               "json-backend-jzon"
               "usocket" "bordeaux-threads" "babel" "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
