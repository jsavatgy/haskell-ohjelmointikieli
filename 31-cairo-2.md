## Piirtokirjasto `Graphics.Rendering.Cairo`

### Komennot

- Piirtokomennot:
  - `renderWith`, 
`save`, 
`restore`, 
`status`, 
`withTargetSurface`, 
`pushGroup`, 
`pushGroupWithContent`, 
`popGroupToSource`, 
`setSourceRGB`, 
`setSourceRGBA`, 
`setSource`, 
`setSourceSurface`, 
`getSource`, 
`setAntialias`, 
`setDash`, 
`setFillRule`, 
`getFillRule`, 
`setLineCap`, 
`getLineCap`, 
`setLineJoin`, 
`getLineJoin`, 
`setLineWidth`, 
`getLineWidth`, 
`setMiterLimit`, 
`getMiterLimit`, 
`setOperator`, 
`getOperator`, 
`setTolerance`, 
`getTolerance`, 
`clip`, 
`clipPreserve`, 
`clipExtents`, 
`resetClip`, 
`fill`, 
`fillPreserve`, 
`fillExtents`, 
`inFill`, 
`mask`, 
`maskSurface`, 
`paint`, 
`paintWithAlpha`, 
`stroke`, 
`strokePreserve`, 
`strokeExtents`, 
`inStroke`, 
`copyPage`, 
`showPage`. 
- Polut:
  - `getCurrentPoint`, 
`newPath`, 
`closePath`, 
`arc`, 
`arcNegative`, 
`curveTo`, 
`lineTo`, 
`moveTo`, 
`rectangle`, 
`textPath`, 
`relCurveTo`, 
`relLineTo`, 
`relMoveTo`.
- Kuviointi (`~ = pattern-/-Pattern`):
  - `withRGB~`, 
`withRGBA~`, 
`with~ForSurface`, 
`withGroup~`, 
`withLinear~`, 
`withRadial~`, 
`~AddColorStopRGB`, 
`~AddColorStopRGBA`, 
`~SetMatrix`, 
`~GetMatrix`, 
`~SetExtend`, 
`~GetExtend`, 
`~SetFilter`, 
`~GetFilter`.
- Koordinaattimuunnokset:
  - `translate`, 
`scale`, 
`rotate`, 
`transform`, 
`setMatrix`, 
`getMatrix`, 
`identityMatrix`, 
`userToDevice`, 
`userToDeviceDistance`, 
`deviceToUser`, 
`deviceToUserDistance`.
- Teksti:
  - `selectFontFace`, 
`setFontSize`, 
`setFontMatrix`, 
`getFontMatrix`, 
`setFontOptions`, 
`showText`, 
`fontExtents`, 
`textExtents`.
- Kirjasin (`~ = fontOptions-`):
  - `~Create`, 
`~Copy`, 
`~Merge`, 
`~Hash`, 
`~Equal`, 
`~SetAntialias`, 
`~GetAntialias`, 
`~SetSubpixelOrder`, 
`~GetSubpixelOrder`, 
`~SetHintStyle`, 
`~GetHintStyle`, 
`~SetHintMetrics`, 
`~GetHintMetrics`.
- Pinnat (`~ = surface-/-Surface`):
  - `withSimilar~`, 
`createSimilar~`, 
`renderWithSimilar~`, 
`~GetFontOptions`, 
`~Finish`, 
`~Flush`, 
`~MarkDirty`, 
`~MarkDirtyRectangle`, 
`~SetDeviceOffset`.
- Kuvapinnat (`~ = imageSurface-/-ImageSurface`)
  - `with~`, 
`with~ForData`, 
`formatStrideForWidth`, 
`create~ForData`, 
`create~`, 
`~GetWidth`, 
`~GetHeight`, 
`~GetFormat`, 
`~GetStride`, 
`~GetData`, 
`~GetPixels`.
- Png-kuvatiedostot:
  - `with~FromPNG`, 
`~CreateFromPNG`, 
`surfaceWriteToPNG`.
- Pdf-pinnat:
  - `withPDFSurface`, 
`pdfSurfaceSetSize`.
- PostScript-pinnat:
  - `withPSSurface`, `psSurfaceSetSize`.
- Svg-vektorikuvat:
  - `withSVGSurface`.
- Alueet (`~ = region-`):
  - `~Create`, 
`~CreateRectangle`, 
`~CreateRectangles`, 
`~Copy`, 
`~GetExtents`, 
`~NumRectangles`, 
`~GetRectangle`, 
`~IsEmpty`, 
`~ContainsPoint`, 
`~ContainsRectangle`, 
`~Equal`, 
`~Translate`, 
`~Intersect`, 
`~IntersectRectangle`, 
`~Subtract`, 
`~SubtractRectangle`, 
`~Union`, 
`~UnionRectangle`, 
`~Xor`,  
`~XorRectangle`.
- Muut komennot:
  - `liftIO`, 
`version`, 
`versionString`. 

### Tietotyypit

```haskell
Render 
Matrix
Surface
Pattern
Status
Operator 
Antialias 
FillRule
LineCap
LineJoin
ScaledFont
FontFace
Glyph
TextExtents 
FontExtents 
FontSlant
FontWeight
SubpixelOrder 
HintStyle 
HintMetrics 
FontOptions
Path
RectangleInt 
RegionOverlap
egionOverlapPart
Region
Content
Format
Extend
Filter 
```

