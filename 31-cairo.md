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
- Kuviointi:
  - `withRGBPattern`, 
`withRGBAPattern`, 
`withPatternForSurface`, 
`withGroupPattern`, 
`withLinearPattern`, 
`withRadialPattern`, 
`patternAddColorStopRGB`, 
`patternAddColorStopRGBA`, 
`patternSetMatrix`, 
`patternGetMatrix`, 
`patternSetExtend`, 
`patternGetExtend`, 
`patternSetFilter`, 
`patternGetFilter`.
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
- Kirjasin:
  - `fontOptionsCreate`, 
`fontOptionsCopy`, 
`fontOptionsMerge`, 
`fontOptionsHash`, 
`fontOptionsEqual`, 
`fontOptionsSetAntialias`, 
`fontOptionsGetAntialias`, 
`fontOptionsSetSubpixelOrder`, 
`fontOptionsGetSubpixelOrder`, 
`fontOptionsSetHintStyle`, 
`fontOptionsGetHintStyle`, 
`fontOptionsSetHintMetrics`, 
`fontOptionsGetHintMetrics`.
- Pinnat:
  - `withSimilarSurface`, 
`createSimilarSurface`, 
`renderWithSimilarSurface`, 
`surfaceGetFontOptions`, 
`surfaceFinish`, 
`surfaceFlush`, 
`surfaceMarkDirty`, 
`surfaceMarkDirtyRectangle`, 
`surfaceSetDeviceOffset`.
- Kuvapinnat:
  - `withImageSurface`, 
`withImageSurfaceForData`, 
`formatStrideForWidth`, 
`createImageSurfaceForData`, 
`createImageSurface`, 
`imageSurfaceGetWidth`, 
`imageSurfaceGetHeight`, 
`imageSurfaceGetFormat`, 
`imageSurfaceGetStride`, 
`imageSurfaceGetData`, 
`imageSurfaceGetPixels`.
- Png-kuvatiedostot:
  - `withImageSurfaceFromPNG`, 
`imageSurfaceCreateFromPNG`, 
`surfaceWriteToPNG`.
- Pdf-pinnat:
  - `withPDFSurface`, 
`pdfSurfaceSetSize`.
- PostScript-pinnat:
  - `withPSSurface`,  
`psSurfaceSetSize`.
- Svg-vektorikuvat:
  - `withSVGSurface`.
- Alueet:
  - `regionCreate`, 
`regionCreateRectangle`, 
`regionCreateRectangles`, 
`regionCopy`, 
`regionGetExtents`, 
`regionNumRectangles`, 
`regionGetRectangle`, 
`regionIsEmpty`, 
`regionContainsPoint`, 
`regionContainsRectangle`, 
`regionEqual`, 
`regionTranslate`, 
`regionIntersect`, 
`regionIntersectRectangle`, 
`regionSubtract`, 
`regionSubtractRectangle`, 
`regionUnion`, 
`regionUnionRectangle`, 
`regionXor`,  
`regionXorRectangle`.
- Muut komennot:
  - `liftIO`, 
`version`, 
`versionString`. 

### Tietotyypit

```haskell
data Render m
data Matrix
data Surface
data Pattern
data Status
data Operator = OperatorClear | OperatorSource 
  | OperatorOver | OperatorIn | OperatorOut | OperatorAtop 
  | OperatorDest | OperatorDestOver | OperatorDestIn 
  | OperatorDestOut | OperatorDestAtop | OperatorXor 
  | OperatorAdd | OperatorSaturate | OperatorMultiply 
  | OperatorScreen | OperatorOverlay | OperatorDarken 
  | OperatorLighten | OperatorColorDodge 
  | OperatorColorBurn | OperatorHardLight 
  | OperatorSoftLight | OperatorDifference 
  | OperatorExclusion | OperatorHslHue 
  | OperatorHslSaturation | OperatorHslColor 
  | OperatorHslLuminosity
data Antialias = AntialiasDefault | AntialiasNone 
  | AntialiasGray | AntialiasSubpixel | AntialiasFast 
  | AntialiasGood | AntialiasBest
data FillRule = FillRuleWinding | FillRuleEvenOdd
data LineCap = LineCapButt | LineCapRound | LineCapSquare
data LineJoin = LineJoinMiter | LineJoinRound 
  | LineJoinBevel
data ScaledFont
data FontFace
data Glyph
data TextExtents = TextExtents { 
  textExtentsXbearing, textExtentsYbearing, 
  textExtentsWidth, textExtentsHeight, 
  textExtentsXadvance, textExtentsYadvance }
data FontExtents = FontExtents { 
  fontExtentsAscent, fontExtentsDescent, 
  fontExtentsHeight, fontExtentsMaxXadvance, 
  fontExtentsMaxYadvance }
data FontSlant = FontSlantNormal | FontSlantItalic 
  | FontSlantOblique
data FontWeight = FontWeightNormal | FontWeightBold
data SubpixelOrder 
data HintStyle = HintStyleDefault | HintStyleNone 
  | HintStyleSlight | HintStyleMedium | HintStyleFull
data HintMetrics = HintMetricsDefault | HintMetricsOff 
  | HintMetricsOn
data FontOptions
data Path
data RectangleInt = RectangleInt { x, y, width, height }
data RegionOverlap = RegionOverlapIn | RegionOverlapOut 
  | RegionOverlapPart
data Region
data Content = ContentColor | ContentAlpha 
  | ContentColorAlpha
data Format = FormatARGB32 | FormatRGB24 | FormatA8 
  | FormatA1
data Extend = ExtendNone | ExtendRepeat | ExtendReflect 
  | ExtendPad
data Filter = FilterFast | FilterGood | FilterBest 
  | FilterNearest | FilterBilinear | FilterGaussian
```

