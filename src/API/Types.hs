{- | The types the API reads and writes, one module per domain.

They were one module until its size made an optimised build of it take longer
than the release job allows: GHC compiles a module on one core, so the domains
are now separate modules it can compile side by side. Importing @API.Types@
still brings all of them.

Record types derive their JSON and schema instances through
'API.JsonOptions.Stripped', which strips the lowercase field prefix; an
instance always sits next to its type.
-}
module API.Types (
    module API.Types.Activity,
    module API.Types.Authoring,
    module API.Types.Common,
    module API.Types.Comparison,
    module API.Types.Database,
    module API.Types.Impacts,
    module API.Types.MethodComparison,
    module API.Types.MethodEdit,
) where

import API.Types.Activity
import API.Types.Authoring
import API.Types.Common
import API.Types.Comparison
import API.Types.Database
import API.Types.Impacts
import API.Types.MethodComparison
import API.Types.MethodEdit
