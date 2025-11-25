# __ALS__ module

The Alternated Least Squares (ALS) algorithms factorizes the data matrix
given a number of species and some constraints (*e.g.*, positivity of
kinetics…).

The left panel contains the controls, and the right panel displays the
outputs.

## Controls

  - `Dimension` is the number of species. This should be consistent with
    the results of the SVD analysis.

  - `Max # Iter.` is the maximal number of iterations allowed before
    stopping the optimizer.

  - `Log convergence threshold` controls the logarithm of the stopping
    convergence threshold of the optimizer.

  - The `Run` button starts the ALS optimization.

Several tabs enable to fine tune the ALS analysis:

  - `Options` tab
    
      - **Initialization** enables to select a starting point
        
          - `|SVD|` takes the absolute values of the SVD vectors
        
          - `PCA` performs Principal Component Analysis by centering
            the data before decomposition. This can be beneficial when
            the data has significant baseline offsets or when you want
            to focus on variance rather than absolute values.
        
          - `NMF` takes the solution of a Non-negative Matrix
            Factorization algorithm
        
          - `Sequential` performs a series of ALS optimizations with
            increasing dimension
        
          - `Restart` enables to restart from a previous run. It does
            not work if `Dimension` is changed.
    
      - `Use SVD-filtered matrix` uses the noise filtering ability of
        thee SVD reconstruction. The matrix is computed with the
        dimension specified in the `SVD` tab.
    
      - `Opt S first` start by optimizing the spectra vectors, instead
        of the kinetics vectors by default.

  - `S const.` tab: constraints on the spectra vectors
    
      - `S > 0 (All)`: global positivity constraint for all spectra
    
      - `Per-component constraints`: enables individual control of
        positivity for each spectrum. When activated, checkboxes appear
        for each component (S_1, S_2, etc.), allowing you to specify
        which should be positive and which can be negative. This is
        useful for:
        - Difference spectra (e.g., S_1 positive, S_2 can be negative)
        - Mixed systems with absorption and bleaching
        - Decay-Associated Spectra (DAS) analysis
    
      - `S unimodal`: unimodality constraint
    
      - `Normalize`: normalize the spectra:
        
          - `SUM(S) = 1` normalizes the area of the spectra. The default
            is to normalize the intensities.
    
      - `Smooth`: a smoothing factor to get less noisy spectra, used as
        the `span` parameter in the
        [`loess`](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/loess)
        function.
    
      - `Fix spectral shape(s)` Select a `.csv` file with spectra to 
        be constrained. The file should be in the same format
        as the data files (separator, decimal mark) and contain
        a header with the names of the species. The first column
        should contain the wavelength. It is possible to load several
        files. Example: (https://github.com/Saldenisov/SK-Ana/blob/master/data/spectrum_ABC_Kinet.csv)
        
        The spectra are interpolated on the wavl grid of the data matrix.
        By default, the spectra are used as such (hard constraint) which
        is often too strong and results in poor solutions. Activating
        `Soft constraint` enables to input a weight for the similarity
        constraint in the loss function of the ALS. The `logWeight`
        slider enables to tune this weight.
      
      - `Correction Spectra` enables an advanced decomposition mode where
        each fixed spectrum is paired with a correction spectrum to account
        for systematic deviations from the reference shape. See the detailed
        [Correction Spectra](correction_spectra.html) documentation for
        complete usage instructions and examples.
      

  - `C const.` tab: constraints on the kinetics vectors
    
      - `C > 0`: positivity constraint
    
      - `Closure`: imposes the conservation of matter by normalizing the
        sum of the kinetics to 1 (at each delay).
        
        **Warning**: This works only for unimolecular processes and
        could be in conflict with normalization constrains on the
        spectra.
    
      - `Presence matrix` enables to specify the occurrence of
        individual species in different experiments when datasets have
        been delay-tiled. By default the matrix is filled with ones (1).
        Put 0 where a species is not expected to occur. **When finished,
        press `Done`**.

## Outputs

  - `Alternated Least Squares` tab
    
    This shows the convergence message of the ALS code.
    
    **Tip**: For a succesful fit, the lack-of-fit should be versy close
    to the lack-of-fit statistics of a SVD with the same dimension.

  - `Diagnostics` tab
    
    This gives access to several results:
    
      - `Data vs. Model` whichh compares side by side the best fit model
        to the data matrix
    
      - `Residuals`which shows the residuals map and an histogram of the
        residuals compared to the histogram of the data.
    
      - `SVD of residuals` which provides the Singular Values
        Decomposition of the residuals matrix.In the ideal case, all
        vectors should be featureless and appear as pure noise. A normal
        Q-Q plot is provided to assess the normality of the residuals
        distribution.

  - `Kinetics and Spectra` tab
    
    This tab provides zoomable plots of the spectra and the associated
    kinetics, identified by color code. The data can be saved to disk.

  - `Contributions` tab
    
    This tab shows the contribution matrix of each species with its
    weight.

  - `Ambiguity` tab
    
    In most cases, the ALS decomposition is not unique and subject to
    rotational ambiguity. If one transforms/combines the spectra, the
    inverse transformation applied to the kinetics will leave their
    combination unchanged. The range of eligible transformations is
    limited by the varoius constraints on the ALS solutions
    (positivity…).
    
    The algorithm performs a brute force exploration of transformation
    matrices and might require a long time to finish. It returns a
    subset of valid spectra and kinetics, from which one can appreciate
    the level of ambiguity and/or search for better behaved solutions
    than the ones returned by the ALS.
    
    Several controls are available:
    
      - `Pick 2 or 3 vectors`: according to the dimension of the ALS
        decomposition, one has to choose a set of vectors. This might be
        the full set for dimensions 2 and 3, but the algorithm does not
        allow explorations of more than three-vectors transformations.
    
      - `Relative positivity threshold`: because of the noise in the
        data, one has to enable some level on non-positivity in the
        transformed vectors. The slider enables to pick a level.
    
      - `Exploration step`: the step amplitude for the exploration of
        the transformation matrix elements. The smaller the better and
        more accurate, but very small steps might incur very long
        calculations.
    
      - `Start`: click to start process when all other parameters have
        been chosen.
    
      - `Stop`: early stop of the process (it works sometimes…)
    
      - `Save`: save the subset of transformed spectra and kinetics to
        disk
