form: "Test"
  word: "file", "F01.wav"
  word: "out", "F01"
endform

t = 1
point = 11

include utils/trackAutoselectProcedure.praat

outfile$ = "/Users/chunyu/Desktop/min-vowel/02analysis/data/cangnan/" + out$ + ".txt"
writeFileLine: outfile$, "file", tab$, "vowel", tab$, "time ", tab$, "F1", tab$, "F2"

ns = rindex (file$, "/")
nl = length (file$)
fileName$ = mid$ (file$, ns + 1, nl-ns)
baseName$ = fileName$ - ".wav"
Read from file: file$
Read from file: file$ - ".wav" + ".TextGrid"
gender$ = left$ (baseName$, 1)
# high 6500 for female, 7000 for male
# low 4500 for female, 5000 for male
# low 4000, high 6000 for Xiamen M4
# low 5500, high 7000 for Cangnan F5

#low = 5500
#high = 7000
if gender$ = "F"
  low = 4500
  high = 6500
else
  low = 5000
  high = 7000
endif

selectObject: "TextGrid " + baseName$
nInt = Get number of intervals: t
label$ = Get label of interval: t, 2
if label$ <> "p" and label$ <> "t" and label$ <> "k" and label$ <> "s" and label$ <> "C"
  selectObject: "TextGrid " + baseName$
  start = Get start time of interval: t, 2
  end = Get end time of interval: t, 2
else
  selectObject: "TextGrid " + baseName$
  label$ = Get label of interval: t, 3
  start = Get start time of interval: t, 3
  end = Get end time of interval: t, 3
endif

selectObject: "Sound " + baseName$
Extract part: start, end, "rectangular", 1, "no"
Rename: "sample"

# Load procedures necessary for unsupervised execution
# Check this file for information about parameters and outputs

# Ensure all global settings have values
@getSettings
time_step = 0.002
enable_F1_frequency_heuristic = 1
maximum_F1_frequency_value = 1200
enable_F1_bandwidth_heuristic = 0
enable_F2_bandwidth_heuristic = 0
enable_F3_bandwidth_heuristic = 0
enable_F4_frequency_heuristic = 1
minimum_F4_frequency_value = 2900
enable_rhotic_heuristic = 1
enable_F3F4_proximity_heuristic = 1
output_bandwidth = 1
output_predictions = 1
output_pitch = 1
output_intensity = 1
output_harmonicity = 1
output_normalized_time = 1

# procedure parameters
dir$ = "."
lowestAnalysisFrequency = low
highestAnalysisFrequency = high
steps = 20
coefficients = 5
formants = 3
method$ = "burg"
image = 0
current_view = 0
max_plot = 4000
# Leave a formant object named after the sound in the Objects list:
out_formant = 2
out_table = 0
out_all = 0

# run FastTrack
selectObject: "Sound sample"
@trackAutoselect: selected(), dir$, lowestAnalysisFrequency, highestAnalysisFrequency, steps, coefficients, formants, method$, image, selected(), current_view, max_plot, out_formant, out_table, out_all
dist = (end - start) / point
# output results from formant object
for j from 1 to point
  ptime = dist * j
  f1 = Get value at time: 1, ptime, "hertz", "Linear"
  f2 = Get value at time: 2, ptime, "hertz", "Linear"
  appendFileLine: outfile$, baseName$, tab$, label$, tab$, j, tab$, ptime, tab$, f1, tab$, f2
endfor
#selectObject: "TextGrid " + baseName$
#nPoint = Get number of points: 3
#for p from 1 to nPoint
#  selectObject: "TextGrid " + baseName$
#  ptime = Get time of point: 3, p
#  tlabel$ = Get label of point: 3, p
#  ttime = ptime - start
#  selectObject: "Formant sample"
#  f1 = Get value at time: 1, ttime, "hertz", "Linear"
#  f2 = Get value at time: 2, ttime, "hertz", "Linear"
#  appendFileLine: outfile$, baseName$, tab$, tlabel$, tab$, "target", tab$, ttime, tab$, f1, tab$, f2
#endfor

Remove
selectObject: "Sound sample"
Remove

appendInfoLine: baseName$
