# thermoSimFit

GUI Simulation and fitting tools for thermodynamic binding isotherms, based on MM

## Prerequisites

Get your own Mathematica (11/12+) <https://www.wolfram.com/mathematica/>

## Contributions

We are happy for any contributions. ADSDE is the perfect platform for further development and deployment of scientific and educational software. Thus, you are free to fork and dev on your own, or clone and be part of the dev team.

## How to use

### Download and Installation

1. Download the latest release
2. Unpack the zip
3. Open the file **thermoSimFit.nb** (double click)

### Getting started

1. click: Launch Application
2. Choose a program of the suite (click on it)
3. click: Launch Application in the open GUI
4. click: Simulate/Initialize

![](get_started.gif)

### Export data (csv format)

1. click: Generate Saveable data
2. click: save data
3. Choose a directory to Save

### Import raw data (txt) and fit to a model

1. click: import and choose file -> open
2. select appropriate import parameters (JASCO option recognizes meta data header and footer of the brands output)
3. adjust the values for concentrations and binding constants (CBA) according to acquisition conditions
4. give a good guess for the binding constant of interest and the signal parameters
5. adjust the red simulated curve as close as possible to the real data (blue dots)
6. click: Fit
7. results are presented at the end and can be saved
