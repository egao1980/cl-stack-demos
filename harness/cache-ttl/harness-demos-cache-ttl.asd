(defsystem "harness-demos-cache-ttl"
  :version "0.1.0"
  :description "Demo of cache-protocol 0.2.0's in-memory TTL cache, the fixes for cache-protocol#1, #2 and #4: the default clock (+MONOTONIC-TICK+, a :ttl 1 put at a second boundary gone 1.1 s later, sub-second TTLs, a 5 ms TTL never cut short), an injected clock taken as exact (expiry once clock >= put + TTL, as in cachetools), TTL values (fractions, floats, 0, negative, float infinities, NaN as CACHE-ERROR), CACHE-INCR and CACHE-CAS atomic across threads, CACHE-INCR keeping the TTL (a rate-limit window), the Redis INCR text rules and int64 range, and the CACHE-CAS-MISMATCH condition with its USE-VALUE restart"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "cache-protocol" "0.2.0")
               "bordeaux-threads")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "demo")))
