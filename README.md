# Screenshots for issue #1242 (hoSortGlyphAboveCaption)

This branch only holds the screenshots used in the pull request for
[issue #1242](https://github.com/JAM-Software/Virtual-TreeView/issues/1242) and the small
program that renders them. It is not meant to be merged.

`HeaderShots.dpr` paints the header of a `TVirtualStringTree` offscreen with
`TVirtualTreeColumns.PaintHeader` in several variants (Explorer theme, themed, classic,
VCL styles, caption alignments, right-to-left, hot/pressed) and saves the results,
zoomed 3x, as PNG files next to the executable.

Build with `build.bat <path to Virtual TreeView Source folder>` and run `HeaderShots.exe`.
