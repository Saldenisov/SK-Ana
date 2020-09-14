# __Kinet__ module

This module enables to introduce an explicit chemical scheme, leading to
a hybrid model, as described in [Ruckebusch
(2012)](http://dx.doi.org/10.1016/j.jphotochemrev.2011.10.002). The
spectra are optimized by least-squares for each value of the kinetic
parameters, which are optimized by a non-linear algorithm.

The optimizer is based on a Bayesian statistical model where one
maximizes the posterior probability density function (pdf) of the
kinetic parameters, given the data and model.

The left panel contains the controls, and the right panel displays the
outputs.

## Controls

  - `Model` enables to describe the chemical scheme. It contains two
    groups of tabs. At the top, `Type`, `Load` and `Save` manage the
    model and model files. At the bottom, `Scheme`, `Rates`, `Conc.` and
    `Eps.` display the parameters values and enable to edit some of
    them.
    
      - `Type` to enter manually the model in the text box. It must
        contain the reaction scheme and initial values for the reaction
        rates, the initial concentrations of species and their maximum
        extinction coefficients
        
          - **Reactions** are typed one per line, with two segments,
            separated by a semi-column (;). The first segment contains
            the reaction, for instance `A + B -> C + D`, where A, B are
            the reactants, ‘-\>’ the reaction symbol and C, D the
            products. The second segment contains the initial value for
            the reaction rate and its uncertainty factor, separated by a
            slash ‘/’. For instance, `1e8 / 1.2`, meaning a rate
            constant of 1e8 (units should be consistent with your data)
            and a multiplicative uncertainty factor of 1.2, which
            corresponds to a relative uncertainty of about 20%.
            
            The full line for this example is thus `A + B -> C + D ; 1e8
            / 1.2`
            
            **Note(s)**
            
              - to fix a rate constant, its uncertainty factor should be
                1.
            
              - lines starting with a sharp (\#) are treated as comments
            
              - the reaction scheme is the same for all the experiments
                when data have been delay-tiled.
        
          - **Extinction coefficients** are typed in the form `eps_X =
            value / Feps`, where `X` is the name of a species declared
            in the scheme, `value` is the value and `Feps` the
            uncertainty factor on this value.
            
            For instance `eps_A = 0.001 / 3`
            
            **Note(s)**
            
              - the extinction coefficient of all species in the
                recation scheme is 0 by default. The extinction
                coefficients of ‘visible’ species have to be declared.
        
          - **Initial concentrations** are decalred with a format
            similar to the extinction coefficients `c0_X_i = value /
            Fc0`, where `X` is a species, `i` is the index of an
            experiment (should be 1 for single experiments) and `Fc0` is
            an uncertainty factor.
            
            For instance `c0_A_1 = 1 / 1`, meaning that the initial
            concentration of `A` is fixed to 1 in the first (or single)
            experiment.
            
            **Note(s)**
            
              - all initial concentrations are 0 by default.
        
          - **Examples**
            
                # 3-species DAS 
                A -> 0 ; 1 / 3 
                B -> 0 ; 0.5 / 3 
                C -> 0 ; 0.001 / 3 
                
                eps_A = 0.001 / 3 
                eps_B = 0.001 / 3 
                eps_C = 0.001 / 3 
                
                c0_A_1 = 1 / 1 
                c0_B_1 = 1 / 1 
                c0_C_1 = 1 / 1 
            
            or
            
                # Transformation of A to C with a blind intermediate 
                A -> B ; 1 / 3 
                B -> C ; 0.5 / 3 
                eps_A = 0.001 / 3 
                eps_C = 0.001 / 3 
                c0_A_1 = 1 / 1 
        
          - Click `Done` to process the model
    
      - `Load` to load an existing model file
    
      - `Save` to save the typed model file (this does not save the
        optimized values)
    
      - `Scheme` displays the reaction scheme after a model is defined
        (`Type > Done` or`Load`). It is not editable.
    
      - `Rates` displays the reaction rates and their uncertainty
        factors, which are editable.
    
      - `Conc.` displays the initial concentrations of all species, with
        one tab per experiment. All values are editable.
    
      - `Eps.` displays the extinction coefficients of all species. All
        values are editable.
        
        **Note(s)**
        
          - values modified in the `Rates`, `Conc.` or `Eps.` tabs
            affect only the initial values for the optimizer. They are
            not taken into account when saving a model file in `Save`.

  - `Run` contains the controls for the optimizer. By default, the
    optimizer performs a local search around the initial values, in a
    box defined by the parameters uncertainty factors. A global
    optimization can be performed by using multiple random starting
    points within this uncertainty box.
    
      - `Global Optimization Iterations` controls the number of
        iterations of the global optimizer (default 0)
    
      - `Global Population Factor` controls the number of random
        starting points. Active only if the number of lobal iterations
        is not zero.
    
      - `Log Convergence Threshold` enables to tweak the convergence
        threshold os the optimizer.
    
      - `S>0` defines the positivity constraint on spectra. It should be
        unchecked for DAS analysis.
    
      - `Smooth`: a smoothing factor to get less noisy spectra, used as
        the `span` parameter in the
        [`loess`](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/loess)
        function.
    
      - `Weighted data` controls the use of a weighted least-squares
        criterion (experimental…)
    
      - `Restart` controls the initialization of the optimizer with the
        last results (should not be checked at first run)
    
      - `Run` launches the optimized.
    
    **Note(s)**
    
      - It is recommended to make a final run with `Restart` on to check
        that the optimized value is stable
    
      - For a gloal optimization, it is recommended to run several small
        sets of iterations with `Restart` on to ensure a faster
        convergence, rather than a single large set of iterations. This
        avoids a premature trapping of the walkers in a local minimum.

## Outputs

  - `Best Params` tab
    
    It displays the results of the optimization (notably the final
    Lack-of-fit, which can be analyzed in `Diagnostics`) and signals
    possible problems with the solution, for instance when a value is at
    a limit of the initial uncertainty box. Such problems can be
    visualized in the `Identifiability` tab.

  - `Trace` tab
    
    It shows the output and messages of the optimizer.

  - `Identifiability` tab
    
    It presents two tabs to appreciate the identification of the
    parameters:
    
      - `Densities` plots the marginal densities of the Laplace
        approximation of the posterior pdf (salmon) in comparison to the
        prior densities defined by the parameters uncertainty factors
        (blue). If a parameter is well identified, its posterior density
        should be more concentrated than its prior density. Also, the
        posterior density should not be concentrated at a limit of the
        uncertainty box.
    
      - `Sample` displays histograms and pairs scatterplots for
        parameters samples drawn from the Laplace approximation of the
        posterior pdf. The upper panel shows the correlation
        coefficients between the parameters.

  - `Diagnostics` tab
    
    This tab contains a series of tabs
    
      - `Lack-of-fit` presents the level of LOF reached by the solution,
        compared to the level for a SVD decomposition with the same
        number of species.
    
      - `Integ. kinet.` presents the wavelength-integrated optimized
        matrix compared to the data.
    
      - `Data vs. Model` whichh compares side by side the best fit model
        to the data matrix
    
      - `Residuals`which shows the residuals map and an histogram of the
        residuals compared to the histogram of the data.
    
      - `SVD of residuals` which provides the Singular Values
        Decomposition of the residuals matrix. In the ideal case, all
        vectors should be featureless and appear as pure noise. A normal
        Q-Q plot is provided to assess the normality of the residuals
        distribution.

  - `Kinetics and Spectra` tab
    
    This tab provides zoomable plots of the spectra and the associated
    kinetics, identified by color code.
    
      - `Plot uncertainty bands` plots the envelope of the set of
        spectra and kinetics enabled by the final uncertainty on the
        optimized model parameters. These correspond to the min and max
        values generated from a random sample of parameters.
    
      - `Save` the displayed vectors can be saved to disk.

  - `Contributions` tab
    
    This tab shows the contribution matrix of each species with its
    weight.
