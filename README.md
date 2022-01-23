# elm-review-reducible-lambdas

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.

## Provided rules

- [`NoEtaReducibleLambdas`](https://package.elm-lang.org/packages/jsuder-xx/elm-review-reducible-lambdas/1.0.0/NoEtaReducibleLambdas) - Reports REPLACEME.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoEtaReducibleLambdas
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoEtaReducibleLambdas.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jsuder-xx/elm-review-reducible-lambdas/example
```
