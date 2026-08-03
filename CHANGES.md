## unreleased

 * `Property.diff` now records the two values as rendered strings rather than
   a pre-computed diff, and rendering them is a replaceable step. Install a
   renderer with `Property.set_diff_renderer`, or pass one for a single report
   with `Property.format_report ~diff_renderer`. `Property.default_diff_renderer`
   keeps the built-in LCS rendering, which is unchanged.
 * **Breaking:** `Property.failure`'s `diff` field is now
   `Property.diff_values option` (`{ left : string; right : string }`) instead
   of `Diff.t option`. Recover the old value with `Diff.of_strings left right`.
 * Diffs in `check_group` output are now indented to line up with the
   annotations above them.

## 0.1 (Thu 30 Jul 2026)

 * Initial public release.
