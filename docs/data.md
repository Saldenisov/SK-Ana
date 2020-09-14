[__Go to Top__](index.html)

# __Data Selection__ module

This module enables to fine tune a subset of data to be analyzed, mainly
to remove over-noisy area or artefacts (signal rise, Cherenkov…).

**Important**: you have to visit this tab to activate/enable the the
analysis tabs (`SVD`, `ALS`…).

The left panel contains four tabs:

  - `Selection`: to select the external limits of the treated area

  - `Baseline`: to define the areas used to correct the signal baseline

  - `Wavl Mask`: to define masks on the wavelength axis (wavelengths
    ranges not to be analyzed)

  - `Delay Mask`: to define masks on the delay axis, typically to mask
    signal artefacts

and a set of buttons to `Reset`, `Save` and `Load` selections.

**Warning**: the `Save` and `Load` operations are experimental, meaning
unstable. Presently, any difference in the loaded matrices prevents the
reuse of saved selections.

The right panel contains two tabs with graphical representations that
show the modifications due to the actions in the left panel:

  - the `Data` tab represents the data matrix and averaged cuts along
    both axes.
    
      - The data matrix is zoomable (`click and drag` + `dble click`;
        another `dble click` cancels the zoom).
    
      - The cuts position are marked by violet dashed lines and they are
        controlled by two sliders (`Reference wavl` and `Reference
        delay`). Each cuts data can be saved to disk by clicking on the
        corresponding `Save` buttons.
    
      - Masks are represented by grayed areas.

  - the `Cuts` tab provides the usual stacked lines representation, for
    the spectra (left) and kinetic traces (right), where you can choose
    the cut frequency `Cut freq.` for each axis. Both figures are
    zoomable and the corresponding data can be saved to disk.

## __Selection__ tab

This tab contains three sliders:

  - `OD Range`: select the Optical Density range to improve
    visualization, notably when there are spikes. **This has no impact
    on data selection.**

  - `Wavelength Range`: selects the min and max wavelengths of the
    matrix

  - `Delay Range`: selects the min and max delays of the matrix

## __Baseline__ tab

A Baseline mask is used to correct the baseline in a data matrix, by
delaywise averaging the masked values to zero.

When several matrices have been delay-tiled, each baseline correction is
applied to the data between this mask and the next one (or the end if
none is present).

**Note**: The data covered by the Baseline masks are *not* excluded from
the data analysis.

The initial tab contains two elements:

  - a `Nb of masks` input where you can select the desired number of
    masks.

  - a `Auto` button, which attempts to generate and locate the adequate
    number of masks.

For each mask, a slider is created enabling to define its min and max
positions. The masks are represented by salmon transparent areas on the
matrix and cuts figures.

**Tip**: Zooming on the data matrix is helpful to define precise limits.

## __Wavl Mask__ tab

The wavl masks are intended to exclude wavelength-delimited area(s) from
data analysis, typically over-noisy areas or laser wavelengths.

The initial tab contains two elements:

  - a `Nb of masks` input where you can select the desired number of
    masks.

  - a `Auto` button, which attempts to generate and locate the adequate
    number of masks.

For each mask, a slider is created enabling to define its min and max
positions.

## __Delay Mask__ tab

The delay masks are intended to exclude delay-delimited area(s) from
data analysis, typically baseline areas and artefacts (Cherenkov).

The initial tab contains two elements:

  - a `Nb of masks` input where you can select the desired number of
    masks.

  - a `Auto` button, which attempts to generate and locate the adequate
    number of masks.

For each mask, a slider is created enabling to define its min and max
positions.
