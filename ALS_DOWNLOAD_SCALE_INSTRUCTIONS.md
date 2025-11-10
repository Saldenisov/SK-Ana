# ALS Results Download and Spectrum Scale Control

## Summary of Changes

Two new features have been added to the ALS (Alternating Least Squares) output interface:

### 1. Download ALS Results Button

**Location**: ALS Output Vectors tab

**Functionality**:
- Added a green "Download" button next to the existing "Save" button
- When clicked, downloads a ZIP file containing both ALS spectra and kinetics as CSV files
- The ZIP file is named: `{projectTag}_ALS_{nALS}sp_results.zip`
- Contains two CSV files:
  - `{projectTag}_alsSpectra_{nALS}sp.csv`
  - `{projectTag}_alsKinets_{nALS}sp.csv`

**How to use in Docker**:
1. Run your ALS analysis
2. Click the green "Download" button
3. The ZIP file will be downloaded directly to your browser's download folder
4. Extract the ZIP to access the CSV files

### 2. Axis Scale Controls for Both Plots

**Location**: 
- Below the Spectrum plot (left side): 4 input fields
- Below the Kinetics plot (right side): 4 input fields

**Functionality**:
- **Spectrum (S) plot controls**:
  - X min / X max: Control wavelength axis range
  - Y min / Y max: Control spectrum intensity scale
- **Kinetics (C) plot controls**:
  - X min / X max: Control time/delay axis range
  - Y min / Y max: Control concentration scale
- Overrides the auto-zoom functionality when values are set
- Leave empty to use automatic scaling

**How to use**:
1. View your ALS plots
2. Enter desired axis limits in any combination:
   - X min / X max for horizontal axis control
   - Y min / Y max for vertical axis control
3. The plots will update automatically with your custom scales
4. To reset to automatic scaling, clear the fields
5. You can set just X, just Y, or both axes independently

## Files Modified

### UI Files
- `ui_files/ALSOutputVectors.R`
  - Added numeric inputs for Y-axis min/max below the spectrum plot
  - Added download button next to the save button

### Server Files
- `server_files/ALS.R`
  - Updated `output$alsSpVectors` to use custom Y-axis scale when provided
  - Added `output$alsSpKinDownload` download handler
  - Added notification when files are saved using the "Save" button

## Technical Details

### Download Handler Implementation
The download handler (`alsSpKinDownload`):
1. Creates temporary files for spectra and kinetics
2. Writes CSV files with appropriate headers
3. Creates a ZIP archive containing both files
4. Returns the ZIP file to the browser for download

### Y-Axis Scale Implementation
The spectrum plot now:
1. Checks if custom Y-axis values are provided via `input$alsSpYmin` and `input$alsSpYmax`
2. Validates that both values are finite and min < max
3. Uses custom scale if valid, otherwise falls back to zoom/auto-scale behavior
4. Works seamlessly with the existing double-click zoom functionality

## Benefits

1. **Browser Download**: No need to access Docker container filesystem - files download directly to browser
2. **Convenient Packaging**: Both spectra and kinetics in one ZIP file
3. **Flexible Visualization**: Full control over spectrum Y-axis scale for better data interpretation
4. **Backward Compatible**: Existing "Save" functionality remains unchanged

## Usage in Docker Environment

The download functionality works seamlessly in Docker containers because:
- The download is handled through the browser via Shiny's `downloadHandler`
- No need to map volumes or access container filesystem
- Files are streamed directly to the browser's download location
- Works the same way as the existing "Download Report" and "Get my files" features
