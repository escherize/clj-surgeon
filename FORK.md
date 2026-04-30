# clj-surgeon (Metabase fork)

This is Bryan's fork of [realgenekim/clj-surgeon](https://github.com/realgenekim/clj-surgeon)
with extended form-classification that recognizes Metabase's macros — `mu/defn`,
`defenterprise`, `defsetting`, `defendpoint`, and friends — across `:ls`,
`:deps`, `:ls-deps`, `:ls-extract`, `:topo`, and the structural ops that build
on them.

Pitch upstream: TODO (link to issue once filed).

## Install

```bash
git clone -b mu-defn-and-shared-classifier \
  https://github.com/escherize/clj-surgeon.git \
  ~/dv/clj-surgeon
cd ~/dv/clj-surgeon
make install   # → ~/bin/clj-surgeon
```

Make sure `~/bin` is on your `PATH`. If you previously installed upstream
clj-surgeon, this overwrites it (same target path).

## Configuration

**For Metabase: nothing.** The classifier reads Metabase's existing
`.clj-kondo/config.edn` and picks up `:lint-as` / `:hooks/:analyze-call`
entries for `defenterprise`, `defsetting`, `defendpoint`, etc.
Aliased forms like `mu/defn`, `mu/defn-`, `s/defn` are recognized by a
suffix rule, also without config.

For other projects, drop a `.clj-surgeon.edn` at the repo root if needed:

```edn
{:def-forms     #{"my-lib/defentity" "my-lib/defcomponent"}
 :arglist-forms #{"my-lib/defentity"}}   ; defaults to :def-forms
```

## What works on Metabase

```bash
clj-surgeon :op :ls :file src/metabase/query_processor/card.clj
# (mu/defn forms now have :name and :args, defenterprise gets :name, etc.)

clj-surgeon :op :ls-deps :file src/metabase/query_processor/api.clj :form run-streaming-query
# (the dep graph now includes mu/defn callees)

clj-surgeon :op :topo :file src/metabase/query_processor/pivot.clj
# (no doseq-binding artifacts; respects mu/defn forms)
```

## Status

Merged with upstream `main` as of the latest push — the classifier and
upstream's reader-conditional walking compose cleanly. All 133 tests
(446 assertions) pass.

## Reverting to upstream

```bash
cd ~/dv/clj-surgeon
git remote add upstream https://github.com/realgenekim/clj-surgeon.git
git fetch upstream
git checkout main
git reset --hard upstream/main
make install
```
