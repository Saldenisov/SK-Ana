# __Project__ module

Project definition and data input.

- [__New Project__ tab](#new-project-tab)
- [__Open__ and __Save__ tabs](#open-and-save-tabs)

## __New Project__ tab

  - **`Project Name`**: choose a name. If not, a name will be generated
    from the datafiles selected below.

  - **`Predefined File Formats`**: a few data file formats have been
    predefined from the datafiles of different experiments. Choose the
    one corresponding to your data. For fine tuning, select ‘Other…’
    which will open a new panel.
    
    |        | Header | Separator | Decimal | Data structure |
    | ------ | ------ | --------- | ------- | -------------- |
    | CSV    | FALSE  | ‘,’       | ‘.’     | wxd            |
    | ELYSE  | FALSE  | ‘\\t’     | ‘.’     | wxd            |
    | Fluo   | FALSE  | ‘;’       | ‘.’     | wxd            |
    | Streak | TRUE   | ‘,’       | ‘.’     | wxd            |
    

      - `Header`: does the first line contain column headers ?
    
      - `Separator`: symbol used to separate the columns
    
      - `Decimal`: character used in the file for decimal points
    
      - `Data structure`:
        
          - `wxd`: wavelength in columns; delays in lines
        
          - `dxw`: delays in columns; wavelengths in lines
    
    **Note**: the first line of the matrix must contain the delays or
    wavelengths, depending on the choice of `Data structure`.
    
    **Example** of a CSV-type data file structure
    
        'x/y', t1,  t2,  t3,  t4, ...
        wl1,  x11, x12, x13, x14, ...
        wl2,  x21, x22, x23, x24, ...
        ...

  - **`Load-time compression factors`**: the data can be averaged by
    blocks at load time to save processing time and reduce noise.
    
      - `Delay` width (in pixels) of the block in delay dimension
    
      - `Wavl` width (in pixels) of the block in wavelength dimension

  - **Transform delay (for single matrix only)**: when multiple files are
  assembled (tiled) the delay scale is replaced by an index in the plots
  to keep an increasing value. When a single matrix is downloaded, the
  original delay scale is preserved. This can be overridden by choosing
  either an `index` scale or a `Log10` scale. In the latter case, all data 
  points with null or negative delays are recast uniformly in the interval
  $]0, d_{min}[$, where $d_{min}$ is the smallest positive delay.  
  
  
  - **`Select data file(s)`**: select one or several files to be
    analyzed. Selecting the files will create new items in the right
    panel:
    
      - a success message ‘**Data Loaded \!**’
    
      - an active table with a description of the file(s). When several
        files have been loaded:
        
          - it is possible to use the table to select a subset or to
            reorder them.
        
          - a menu appears with processing options:
            
              - `Average`: average the selected files
            
              - `Tile Wavl`: assemble the matrices in the wavelength
                dimension
            
              - `Tile Delay` (default): assemble the matrices in the
                delay dimension. In this case, the delay coordinate is
                replaced by an index.
            
              - press on **`Do It!`** to process the data
    
      - a summary of the processed matrix
    
      - `Save Matrix`: to save the processed matrix in a .csv file
    
      - a vignette of the processed matrix

  - **`Post-process compression factor`**: the block averaging is
    performed *after* the data files are assembled.

## __Open__ and __Save__ tabs

These are placeholders. The functionalities are not active.
