# SK-Ana Debug Console Guide

## Overview

A new **Debug Console** tab has been added to SK-Ana to help diagnose and monitor application behavior, especially for investigating ambiguity explorer issues.

## ✨ What's New

### Debug Console Tab
- **Location:** Main navigation bar (last tab, after "About")
- **Icon:** Terminal icon
- **Real-time logging:** All events logged with timestamps
- **Auto-refresh:** Console updates every 500ms

### Logging Levels

| Level | Use | Example |
|-------|-----|---------|
| **INFO** | Important milestones | "Ambiguity explorer started" |
| **DEBUG** | Detailed diagnostic info | "Selected vectors: 1,2,3" |
| **WARN** | Warnings/unexpected conditions | "No solutions found" |
| **ERROR** | Errors and exceptions | "Ambiguity explorer error: ..." |

## 🔍 Troubleshooting Ambiguity Explorer

### If screen goes gray when starting ambiguity:

1. **Open Debug Console tab**
2. **Look for log messages** showing:
   - What parameters were used
   - Whether the function was called correctly
   - Any error messages that were thrown

### Example log output (successful):
```
[2025-11-12 12:15:30] [INFO] ===== AMBIGUITY EXPLORER START =====
[2025-11-12 12:15:30] [INFO] ALS results available
[2025-11-12 12:15:31] [DEBUG] Selected vectors: 1,2
[2025-11-12 12:15:31] [DEBUG] Eps: -0.01 Dens: 0.05
[2025-11-12 12:15:31] [DEBUG] Using function: rotAmb2
[2025-11-12 12:15:31] [INFO] Ambiguity explorer process started
[2025-11-12 12:16:15] [INFO] Ambiguity explorer completed: 5 solutions found
```

### Example log output (error):
```
[2025-11-12 12:15:30] [ERROR] Too many vectors selected (max 3)
[2025-11-12 12:15:32] [ERROR] Ambiguity explorer error: rotAmb function failed
```

## 📋 Log Features

### Clear Logs
- **Button:** "Clear Logs" in console header
- **Effect:** Removes all log messages
- **Note:** A new message is logged after clearing

### Download Logs
- **Button:** "Download Logs" in console header
- **Format:** Text file with timestamp
- **Filename:** `sk-ana-debug-YYYYMMDD_HHMMSS.log`
- **Use:** Save logs for later analysis or sharing with developers

### System Information
- **R Version:** Current R installation
- **Platform:** Operating system info
- **Working Directory:** Current project path
- **Available RAM:** System memory
- **Application Started:** Session start time
- **Total Log Lines:** Number of log entries

## 🎯 Events That Trigger Logging

### ALS Analysis
- ALS run started
- Results available
- Iterations completed

### Ambiguity Explorer
- ✅ Run button clicked
- ✅ Vector selection logged
- ✅ Parameters logged (Eps, Dens)
- ✅ Function selection logged
- ✅ Process started
- ✅ Completed with solution count
- ✅ Errors captured and displayed

### Correction Spectra (if enabled)
- Correction mode enabled/disabled
- Lambda parameter value
- Fixed spectra loaded

## 📊 Common Issues & Solutions

### "Screen goes gray" while ambiguity runs

**Check log for:**
1. Did "Ambiguity explorer process started" appear?
   - YES → Process is running, wait or monitor progress
   - NO → Error prevented startup, check for ERROR lines

2. Are there ERROR messages?
   - YES → Read error message and adjust parameters
   - NO → Process may be long-running, increase timeout

3. Look for "Ambiguity explorer completed"
   - YES → Results should appear when loading finishes
   - NO → Process may still be running

### Solution count is 0
```
[...] [INFO] Ambiguity explorer completed: 0 solutions found
[...] [WARN] No solutions found - finished
```
**Action:** Try adjusting parameters:
- Decrease "Exploration Step" (smaller = more detailed)
- Increase "Relative positivity threshold" (less strict)

### Wrong function used
```
[...] [DEBUG] Using function: rotAmb2
```
**Check:** Did you select the correct number of vectors (2 or 3)?

## 🔧 Technical Details

### Log Storage
- **Storage:** In-memory reactive value (debug_logs$messages)
- **Capacity:** Last 500 lines retained
- **Lifetime:** Cleared on app refresh
- **Performance:** Minimal overhead (~1-2ms per log)

### Logging Functions
```r
log_info(msg)      # General info
log_debug(msg)     # Detailed diagnostics
log_warning(msg)   # Warnings
log_error(msg)     # Errors
```

### Files Involved
- **`server_files/debug_console.R`** – Logging system
- **`server_files/ALS.R`** – Ambiguity logging calls
- **`ui.R`** – Debug tab UI
- **`server.R`** – Source debug_console.R

## 📱 Mobile/Responsive

The debug console works on all screen sizes:
- **Desktop:** Full-width console with 400px height
- **Tablet:** Responsive layout, still readable
- **Mobile:** Vertical layout, horizontal scroll for logs

## 🚀 Best Practices

1. **When reporting issues:**
   - Take a screenshot of the debug console
   - Download logs and attach to report
   - Include steps to reproduce

2. **When testing:**
   - Keep debug console open
   - Monitor for ERROR or WARN messages
   - Note timing of operations

3. **Performance:**
   - Clear logs periodically if app feels slow
   - Console update is non-blocking
   - Logs don't affect computation

## ⚙️ Future Enhancements

Potential additions:
- [ ] Log levels filter (show only ERROR, etc.)
- [ ] Search/grep within logs
- [ ] Export logs in JSON format
- [ ] Live process monitoring (CPU, memory)
- [ ] Performance metrics
- [ ] Error stack traces

---

## Quick Start

1. **Open Debug Console tab**
2. **Perform your analysis** (ALS, ambiguity, etc.)
3. **Check logs** for any ERROR or WARN messages
4. **Download logs** if needed for analysis
5. **Clear logs** when done

---

**Version:** 1.0  
**Added:** 2025-11-12  
**Status:** Active and monitoring ✅
