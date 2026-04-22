# __Debug Console__ module

The Debug Console provides real-time diagnostics and logging capabilities
for monitoring SK-Ana's internal operations and troubleshooting issues.

## Overview

This module displays:

- Real-time logs from all SK-Ana processes
- ALS optimization progress and convergence messages
- Ambiguity explorer status updates
- System events and warnings
- Performance metrics

## Features

### Real-Time Logging

All major operations in SK-Ana write diagnostic messages to the debug
console, including:

- **Data loading and processing**: File format detection, matrix assembly,
  compression operations
  
- **SVD analysis**: Decomposition progress, singular value computations
  
- **ALS optimization**: 
  - Iteration progress and convergence status
  - Constraint application (positivity, unimodality, etc.)
  - Lack-of-fit evolution
  - Correction spectra coupling (when enabled)
  
- **Ambiguity exploration**: Transformation matrix exploration progress
  
- **Kinetic modeling**: 
  - ODE integration status
  - Parameter optimization progress
  - Global search iterations

### System Information

The console displays system information including:

- R version and platform
- Loaded packages and versions
- Memory usage
- Session information

## Usage

### Accessing the Debug Console

Click the **Debug Console** tab in the main navigation bar. The console
icon (terminal symbol) helps identify this tab quickly.

### Reading Log Messages

Log messages are displayed in chronological order with timestamps. Message
types include:

- **INFO**: Normal operation messages
- **WARNING**: Non-critical issues that may affect results
- **ERROR**: Critical issues that prevent operation completion
- **DEBUG**: Detailed diagnostic information for troubleshooting

### Troubleshooting with the Console

#### ALS Convergence Issues

If ALS fails to converge or produces unexpected results:

1. Check the console for warning messages about:
   - Constraint conflicts (e.g., positivity violations)
   - Matrix conditioning issues
   - Initialization problems

2. Look for iteration messages showing lack-of-fit progression

3. Verify that correction spectra coupling is working correctly (if enabled)

#### Performance Monitoring

The console helps identify performance bottlenecks:

- Long computation times in specific modules
- Memory allocation warnings
- Excessive iterations in optimization loops

#### Data Loading Problems

If data files fail to load or produce incorrect results:

1. Check console messages for:
   - File format detection results
   - Parsing errors or warnings
   - Matrix dimension mismatches

2. Verify that the correct format parameters were detected

## Best Practices

- **Keep the console open** during long-running operations (ambiguity
  exploration, global optimization) to monitor progress
  
- **Save console output** when reporting bugs: copy the relevant log
  messages to include in bug reports
  
- **Check for warnings** after each analysis step to catch potential issues
  early
  
- **Use system info** to verify package versions when reproducing results

## Technical Notes

- The console uses reactive output that updates automatically as new
  messages are logged
  
- Console output is stored in memory and cleared when the session ends
  
- Very long operations may produce extensive logs; the console automatically
  scrolls to show the most recent messages

---

**See also:**

- [About](about.html) for version information and bug reporting
- [ALS](als.html) for optimization diagnostics
- [Kinet](kinet.html) for kinetic model troubleshooting
