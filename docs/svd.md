# `SVD` module

This module provides the Singular Values Decomposition of the selected
data. The main utility of SVD is to inform us on the complexity of the
data matrix.

For more details about the method, see
[SVD](https://en.wikipedia.org/wiki/Singular_value_decomposition) in
Wikipedia.

The left panel contains two control inputs:

  - `SVD parameters`: the `Dimension` selector enables to select the
    number of singular values used to build figures in the `Data vs.
    Model`, `Residuals` and `Contributions` tabs of the right panel.

  - `Glitch removal in kinetics` enables to remove spikes in the data
    from the visualization of singular vectors in tab `Vectors`
    
      - `Level` is the index of the target **delay** vector from which
        the spike is to be removed.
    
      - the `Clean` button removes the spike. The code masks the point
        with the largest absolute value in the selected vector.
    
      - the `Cancel` button cancels the last spike removal

The right panel contains a set of tabs covering different aspects of the
results:

  - `Singular Values` contains two figures
    
      - the spectrum of singular values (golden dots, dotted blue line)
        and a baseline of noise estimated from the largest singular
        values (violet dashed lines). The number of species that can be
        identified in the data is given by the index of the smallest
        singular value standing out of the noise.
    
      - the lack-of-fit spectrum, which gives the percentage of the
        signal that is not represented depending on the number of
        singular vectors used in the signal recomposition. One can
        appreciate on this graph how adding a new species improves the
        model. For large indexes, on gets the noise-to-signal ratio.
    
    **Rq**: except for ideal data matrices, there is always an ambiguity
    of plus or minus one species (at best) on the cutting level from
    both figures. One gets rather a clear indication on the largest
    decomposition that would *not* be acceptable.

  - `Vectors` presents the wavelength-wise and delay-wise singular
    vectors. The idea here is to discard vectors that contain pure
    noise. Here again, the step from signal to noise is often not
    clearcut.
    
    Spikes in the data matrix can create artificial signal, and one can
    remove the spikes in the *decay-wise* vectors by using the `Glitch
    removal in kinetics` tool in the left panel.

  - `Data vs. Model` shows the SVD data recomposition and the original
    matrix side-by-side. The recomposition is driven by the `Dimension`
    parameter entered in the left panel.
    
    This for illustration, but in order to appreciate the effects of
    `Dimension` on the quality of the model, it is better to focus on
    the nex tab: `Residuals`

  - `Residuals` shows the difference between the data matrix and its
    reconstruction from SVD vectors, controlled by `Dimension` in the
    left panel.

  - `Contributions` shows the individual components of the SVD
    reconstruction.

  - `Statistics` provides a table of results with the singular values,
    the lack-of-fit and the standard deviation of the residuals, versus
    the number of singular vectors.
