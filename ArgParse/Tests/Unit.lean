import ArgParse.Core.Normalize
import ArgParse.Proofs.Sentinel

namespace ArgParse.Tests

open ArgParse Core ArgParse.Proofs

#guard (normalize ["-n", "5", "--", "file"]).pre = ["-n", "5"]
#guard (normalize ["-n", "5", "--", "file"]).post = ["file"]
#guard (normalize ["alpha", "beta"]).post = []
#guard
  (let tokens := ["-v", "--", "tail"]
   let st := normalize tokens
   tokens = st.pre ++ "--" :: st.post)

end ArgParse.Tests
