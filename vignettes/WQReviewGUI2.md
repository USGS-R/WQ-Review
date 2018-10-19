Introduction
============

This manual describes use of the WQReview 2.0 version. The WQReview R-package allows import of water-quality data from the U.S. Geological Survey’s National Water Information System utilizing ODBC connections. It allows review of those data using a series of interactive graphical interfaces through a graphical user interface (GUI) and tabular output of data. Finally, it allows selection of specific water-quality results for updating of data quality indicator (DQI) codes using QWDATA after data review.

Importing Data Using WQReview
=============================

This section describes the process to import data into the WQReview Graphical User Interface (GUI).

The GUI window will open in your system's default internet browser. **It is required that the GUI be run in Google Chrome.** If the GUI opens in internet explorer, you need to change your system's default browser to Google Chrome by opening Google Chrome and going to settings -&gt; default browser.

Load the WQReview package and open the WQReview GUI from R or RStudio
---------------------------------------------------------------------

1.  Load the WQReview package by typing the following into your R script or command window:

``` r
library(WQReview)
```

1.  To open the GUI run the WQReviewGUI() function

``` r
WQReviewGUI()
```

Load WQReview from the desktop or start menu standalone executable
------------------------------------------------------------------

1.  This requires the WQReview 2.0 executable setup installer program has previously been run on the local computer
2.  Navigate to the start menu WQReview 2.0 folder or desktop icon
3.  Click (start menu) or double-click (desktop icon) the WQReview 2.0 icon
4.  A command window will appear, **Do not close this window, it will disappear when the WQReview browser tab is closed**
5.  The WQReview GUI will launch in an internet browser tab

Data import options
-------------------

Data import is accessed by choosing the "Import from internal NWIS server" option under the "Import/Save Data" tab at the top of the WQReview GUI. The "Import from internal NWIS server" option has 11 main inputs.

There are three options available to choose data to import. You need to use only one of these options to import data:

1.  **Site number**: This input accepts (a) manually entered site number(s). Type the site number in the box and click "Add" or hit keyboard Enter key. You may add as many site numbers as you would like by typing or copying them into the Site number box. Do not separate with commas; hitting the Enter key after each site number entry separates the site numbers. Remove a site number by clicking it and hitting the backspace or delete keys. Alternatively, click in the box and press the backspace key to incrementally remove the numbers starting with the last entry

2.  **Site number file**: This input accepts a file of site numbers. The file must be a single column text file of site numbers. Make sure leading zero's have not been removed if using excel to generate the file.

3.  **Project codes**: This input accepts a 9-digit project code (the last nine digits of USGS project numbers) and will select all data associated with this project code.

There are three options to select parameter codes associated with data you will import. You only need one of these options:

1.  **Parameter codes**: This input takes manually entered parameter codes (pcodes). This input box behaves the same as the site number input box.

2.  **Parameter groups**: This input is used to select NWIS parameter groups to retrieve, rather than explicitly selecting individual pcodes.

3.  **pCode file**: This input takes a file of pcodes. The file must be a single column text file of pcodes. Make sure leading zero's have not been removed if using excel to generate the file.

The remaining options are all mandatory and will bracket the dates used to restrict your data import, the server data is imported from, and the local databases that will be queried for the data import.

1.  **Start date**: The start date for the data import. Samples collected after this date will be retrieved. The default value is 10 years before the current date.

2.  **End date**: The end date for the data import. Samples collected before this date will be retrieved. The default is the current date.

3.  **Server name (DSN)**: The name of your NWIS server data source name (DSN) that was established during ODBC setup.

4.  **Environmental DB number**: The number coresponding to the environmental database. Default is "01"

5.  **QA DB Number**: The number coresponding to the QA sample database. Default is "02"

A basic data import, example for a single site
----------------------------------------------

We will start with a basic data import for a single site by parameter groups.

1.  Double check that your server name and database numbers are correct for your data.
2.  Type a site numer into the site number input box and hit Enter key.
3.  Select "All" parameter groups from the Parameter groups select box to pull data for all parameters or select a specific group of parameters. \*\*Beware, if pulling selected groups of parameters (e.g. "cations") chargebalance may not be calculated.
4.  Leave the date range input unchanged to pull the last 10 years of data or enter a date range in the date range input to pull a subset of the data.
5.  Click the "Get data" button at the bottom left to import the data.
6.  A dialogue box and status bar describing the import process will appear in the bottom right corner of the GUI.
7.  A Microsoft Excel workbook window will pop-up and populate with tabs of reports.
8.  Once data have been retrieved, text will be displayed listing the results of the data import. This process can take a few seconds after the Excel workbook populates to show up.

If you encounter issues with the data import, send an email to <cpenn@usgs.gov> and/or post an issue on the WQReview GitHub page at [WQ-Review Issues](https://github.com/USGS-R/WQ-Review/issues)

We can now begin to work with the data.

Creating Plots in WQReview
==========================

This section will describe the plot types available and their functionality in the WQReview GUI and the various ways to manipulate them.

There are three tabs in WQReview where plots are generated and viewed: Result-level review; Troubleshoot charge balance; and Blanks and Rep comparison. Within these three tabs, there are some common features including symbology, labeling conventions, selecting data to plot, and global plot controls described in sections of those names. Within each tab, there are some unique features described in sections titled for each tab.

Symbology
---------

The symbology in all plots is sensitive to the medium code and remark code. Data colors are mapped to the medium code and point data symbols are mapped to the remark code.

Currently supported medium codes include:

1.  WS
2.  WG
3.  OA
4.  WSQ
5.  WGQ
6.  OAQ

Currently supported remark codes include:

1.  No remark code (listed as "Sample" in legend)
2.  "&lt;"
3.  "&gt;"
4.  "E"

Labelling conventions
---------------------

All axes pertaining to a particular pcode will be labelled with that pcode's NWIS parameter name, which includes the units of measurement. The main title of the plot will either by labelled with the NWIS station name if only one station is plotted, or "multi-site plot" if multiple stations are displayed.

Selecting data to plot
----------------------

Generally, plots are generated by selecting a station or stations from the "stations" box and then a data type from the "Parameter" box (in the Result-level review tab only), and then choosing multisite or facet from the "Multi-site option box". In most cases the plot will be displayed or refreshed automatically. **PLOTS WILL NOT BE DISPLAYED IF A SITE AND DATA TYPE ARE NOT SELECTED EVEN IF ONLY ONE SITE OR DATA TYPE WAS IMPORTED.** Additionally, a plot will not be displayed if there are no data to plot, for example if a particular parameter was not measured at a selected site but is selected for plotting. Plots showing multiple parameters for multiple sites can be generated for certain plot types using the "Multi-site options" box.

Global plot controls
--------------------

### New samples threshold

Point data in some plots can be labeled as "New" data based on when those data were last modified using the "New samples threshold" date input in the "Result-level review" tab. Any data modified after the date indicated in this box will be labeled as "New" in relevant plots, but not all plots. This process helps identify data that has not been reviewed from historic data when the data are displayed on plots other than timeseries plots. The "New samples threshold" chosen on the "Result-level review" tab also applies to the "Troubleshoot charge balance" and "Blanks and Rep comparison" tabs, even if a different "Station" is chosen on those tabs. Note that the "New samples threshold" is determined from when data were last **modified** for result-level plots (e.g. timeseries, seasonal, etc.) not when they were created. Sample-level plots ( in "Troubleshoot charge balance" and "Blanks and Rep comparison" are labeled new according to the sample collection date.

### Record highlights

In the "Result-level review" tab, samples or results will be highlighted with a red halo if that sample or result has been selected in the data table located below the "Refresh from excel" button.

### Toolbar for interactive graphics/plots

Most plots are interactive in that the user can hover on a point to see information or zoom into specific areas of the plot. Boxplots are not interactive.

The tool bar at the top of this section appears on hovering in the area at the upper right-hand corner of all interactive graphics. The tools function as follows:

1.  Camera: Allows you to download a plot as a .png file.
2.  Directional arrows: Allows you to pan grab and move (pan) the graphic.
3.  Plus symbol: Zooms in by fixed amount. Note that you can also select an area of the plot to zoom into by holding down a mouse click and drawing a rectangle around the area of interest when the cursor on the graphic is a simple plus shape.
4.  Minus symbol: Zooms out by fixed amount.
5.  House: Resets the axes after zooming to default extent initially shown.
6.  Right-angle points (Toggle Spike Lines): Inactive tool for WQReview graphics
7.  Large tab (Show data on hover): Shows info and value of pcode for that sample and graph on hovering near the point.
8.  Top and bottom tabs (Compare data on hover): shows value for sample you are near, and associated QA sample, if relevant.

Result-level review tab
=======================

This section will describe the details of each plot in teh Result-level review tab how to control them in the WQReview GUI.

Generate plots
--------------

1.  In the Station box, choose a station or stations from the sites you imported by clicking sequentially on the sites you want to view. Backspace to remove an entry.
2.  Choose a parameter from the drop down list in the Parameter box. You can start to type in constituent name or pcode and the list will narrow to one or a few choices. Click on the choice of interest. Backspace to clear the entry and start over.
3.  Multi-site options: Multisite will plot all data from all sites on one graph. Facet will separate data onto different graphs, one per site.
4.  New samples threshold: Add a date here and samples modified or collected after this date will be labeled on plots as "new", subject to the details described in the "Global Plot Controls, New samples threshold" section.
5.  Choose a parameter to plot in the "Parameter" box. Input pcode, words from the pcode, or choose from drop down list as described in "Creating plots in WQReview, selecting data to plot section.

This page generates timeseries plots at the top, seasonal plots below timeseries plots, parameter-parameter plots below seasonal plots, and parameter boxplot at the bottom. The tool bar described in the "toolbar for interactive graphics/plots" is accessed by hovering in the upper right portion of each plot and clicking on an option. Data options and plot controls for all three sets of plots on this page include:

-   Select one or multiple sites
-   Multi-site options: Plot all sites on one plot (multisite) or plot sites on individual side-by-side plots (facet).
-   Label DQI: Superimposes the sample DQI\_CD over the point

### Timeseries plot

Generates a plot of a selected parameter through time.

#### Uses

-   Identifying general trends at a site through time.
-   Identifying temporal outliers.

#### Data selection options

-   Select only one parameter from the dropdown list or in the left-side data table linked from Excel.

#### Plot controls

-   Add LOESS: Adds a locally weighted scatterplot smooth and shaded 95% confidence interval around the smooth
-   Display hydrograph: displays the instantaneous hydrograph if data are available under parameter code "00061"

### Seasonal plot

Generates a seasonal timeseries of parameter value by day of year (DOY) to examine seasonality in parameter values.

#### Uses

-   Identifying seasonality in parameter values.
-   Identifying seasonal outliers.

#### Data selection options

-   Select only one parameter from the dropdown list or in the left-side data table linked from Excel.

#### Plot controls

-   Add LOESS: Adds a locally weighted scatterplot smooth and shaded 95% confidence interval around the smooth

### Parameter-parameter plot

Generates an x-y plot of a selected parameter vs another selected parameter. The two side-by-side plots default to the selected parameter vs. discharge and vs. Specific Conductance.

#### Uses

-   Identifying relationships between parameters.
-   Identifying outliers in related parameters.

#### Data selection options

-   Select only one parameter from the dropdown list or in the left-side data table linked from Excel, for the Y-axis parameter
-   Select only one parameter from the X-Parameter box

#### Plot controls

-   Add linear fit: Add a linear regression line to the x-y data.
-   Log 10 scale: This plots parameter values on a log10 scale.

### Parameter boxplot

Generates a boxplot of the selected parameter

#### Uses

-   Identifying outliers

#### Data selection options

-   Select only one parameter from the dropdown list or in the left-side data table linked from Excel

#### Plot controls

-   None
-   Plot is not interactive

Troubleshoot charge balance tab
===============================

This tab generates a timeseries plot of charge balance and an x-y plot of sum of cations or anions vs conductance.

### Charge balance timeseries

The top plot is a timeseries plot of charge balance percent difference. The plot is annotated with a horizontal dashed line indicating the range of +- 5% and a solid line indicating the range of +- 10%. Symbol type indicates if enough major elements were analyzed to constitute a roughly complete balance. For example, a sample missing calcium would be given a symbol of "Incomplete" because it is a major constituent in the charge balance, but a sample missing fluoride would still be given a "Complete" symbol because it is only a minor constituent of the charge balance.

#### Uses

-   Obtaining a quick visual snapshot of charge imbalance at a site.
-   Identifying charge imbalance issues with current data and comparing to historic charge balance behavior.

#### Data selection options

-   Select one or multiple sites. If one site is chosen, title of the plot is the name of the site. If multiple sites are chosen, title of plot is "multisite charge balance plot. If multisite facet option is chosen, title of each plot is the name of the sites.

#### Plot controls

-   Multi-site options: Plot all sites on one plot (multisite) or plot sites on individual side-by-side plots (facet).

### Ions vs conductance

This plot is an x-y plot of sum of cations or anions (in meq/L) vs conductance. The ratio of the sum of cations or anions vs specific conductance generally falls within a range of 0.92 - 1.24 for natural waters. These are the values used in QWData for alert limits, see QWData documentation and references therein for further explanation.

At many sites, there will be a roughly linear relationship between the sum of cations or anions vs. conductance. If a point falls above or below the general trend, it may be an indication that the particular group of ions is either too low or too high.

#### Uses

-   Identifying samples that fall outside of the typical range of ratios of ions/specific conductance.
-   Identifying which group of ions, cations or anions, are responsible for charge imbalance issues.

#### Data selection options

-   Select one or multiple sites

#### Plot controls

-   Multi-site options: Plot all sites on one plot (multisite) or plot sites on individual side-by-side plots (facet).

Blanks and Rep comparison tab
=============================

This tab generates a boxplot of blank detections and a boxplot of replicate relative percent difference.

### Comparison of blank detections boxplot

This pane generates a box plot of parameter values for blanks above reporting level, organized by DQI code.

#### Uses

-   Identifying systematic detections in blank samples of unapproved data compared to approved detections.

#### Data selection options

-   Select one or multiple sites
-   Select one parameter

#### Plot controls

-   Multi-site options: Plot all sites on one plot (multisite) or plot sites on individual side-by-side plots (facet).

### Repliacte agreement boxplot

This pane generates a plot of replicate relative percent difference for all replicates analyzed at a given site, organized by DQI code. **READ THE FOLLOWING ON REPLICATE PAIRING**

Replicate pairs are identified as follows:

1.  An environmental sample is a sample with medium code of WS or WG and a sample type of 7.
2.  A replicate sample is a sample with a medium code of WSQ or WGQ and a sample type of 7.
3.  An environmental-replicate pair is defined as a replicate sample collected at the same site as an environmental sample on the same day.

**NOTE THAT CENSORED RESULTS ARE GIVEN THE CENSORED VALUE FOR CALCULATIONS. DO NOT INTERPRET REPLICATE ANALYSES BASED ENTIRELY OFF OF THE RELATIVE PERCENT DIFFERENCE PLOT. SEE REPLICATE TABLE AS WELL.**

#### Uses

-   Assessing general replicate agreement at a site for multiple parameters.
-   Comparing current replicate analyses to historic agreement.

#### Data selection options

-   Select one or multiple sites
-   Select one parameter

#### Plot controls

-   Multi-site options: Plot all sites on one plot (multisite) or plot sites on individual side-by-side plots (facet).

Tables
======

This section describe the various tables generated and output to an Excel Workbook by the WQReview GUI.

Auto-flagged samples
--------------------

During data import, samples or results populate a Microsoft excel workbook where they are auto-flagged following criteria developed by NAWQA for all water-quality data that are defined in each tab description or at the end of this section. For any flags comparing a sample that needs review to historic data, the current sample is compared only to all REVIEWED AND ACCEPTED data in the time frame used to import data into WQReview, rather than compared to all prior samples. This is because other data that have not been reviewed and accepted could themselves be problematic and should not be included in the comparison with other samples or results that need review. Flags are displayed in the following tabs: \* DQI needs review \* Chemical sense \* Charge balance issues \* Whole vs Par \* Pesticide flags \* Replicate table \* Blank table \* All charge balance

### DQI needs review

This tab contains all samples in the data pull with a DQI code S, I, or P, and any flags based on comparisons to approved data (see NWIS Flags descriptions at the end of this section). This tab is dynamically linked to the Chrome browser GUI Result-level review tab. Samples in the Excel tab can be sorted or deleted and the resulting format will appear in the lower lefthand table in Chrome when the "Refresh from excel" button is clicked. Filtering on columns in Excel is not recommended as it is not dynamically linked and can cause errors if rows are deleted while in a filtered state.

### Ready for DQI change

This tab starts with column headers and empty rows. As records are reviewed in the "Unapproved data" tab, they can be copied and pasted into this tab, with columns for the new reviewed DQI code and comments. Any records in the tab will be used to create qwsample and qwresult tabs in Excel when the "Generate batch files" button is clicked in the WQReview GUI in Chrome.

### Chemical sense

This table contains basic chemical checks similar to those provided by the QWDATA unix software. The first 7 columns contain sample-level information and the remaining columns contain flags with the column name coorresponding to the particular data check. If a sample fails a given data check, a flag is applied to the respective column for that sample. See NWIS Flags descriptions at the end of this section.

### Charge balance issues

This tab contains flags related to charge balance check 30.21 described in the NWIS Flags descriptions at the end of this section. It is also dynamically linked to the table found under the "Troubleshoot charge balance" tab in the WQRevie GUI inChrome.

### Whole vs Part

This tab compares filtered and unfiltered constituents and flags results based on comparison of filtered and unfiltered result values.

### Pesticide flags

This table contains flags for evaluating the number of pesticide hits in schedule 2437. If there are too many or too few hits in the current sample compared to historic data, the sample will be flagged.

### Replicate table

This table contains all replicate pairs based on the same replicate coding criteria as the Replicate agreement boxplot described in the previous section. Samples are flagged based on whether Relative Percent Difference between the replicates exceeds 10% and whether comparison of replicates is affected by one or both values having a remark code.

### Blank table

This table summarizes all blank samples in the pulled data, organized by parameter. The BD90.90 column calculates the BD90.90 value of the blank which is a value calculated when there are repeated hits on a blank. It is the blank sample concentration corresponding to the 90th percentile of the blank hit values at least the 90 percent confidence level. In other words, there is at least a 90 percent chance that at least 90 percent of all environmental samples will have contamination less than the BD90.90 value for a particular analyte. This calculation is based on USGS SIR 2012-5139 (<https://pubs.usgs.gov/sir/2012/5139/>) and can be useful in situations where there is unexplained, or recurring contamination of the blank samples.

### All charge balance

This table is a "wide" format of all sample charge balance data, including cation and anion sums, individual constituent values, and general flags for samples with charge imbalance.

### Data by result

This table contains all data pulled from NWIS in the "long" format, ie. by record number, parameter code, and result value.

### Data by sample

This table contains all data pulled from NWIS in the "wide" format, ie. by record number date/time and all associated parameter result values.

### qwsample and qwresult

These tabs will be present when the "Generate batch files" button is pressed in the WQReview GUI in Chrome. See "Ready for DQI change" above. The tabs can copied and saved into text files for modifying DQI codes and comments using QWDATA. To regenerate the tabs if more results have been added to the "Ready for DQI change" tab, delete the current qwsample and qwresult tabs and hit the "Generate batch files" button again.

NWIS Flag descriptions
----------------------

### SC\_badLabVSField

The difference between Lab SC (90095) and field SC (00095) is greater than 10%

### 30.01.

pH lower than expected Every water-quality sample (environmental and QC) had its pH (p00400) checked for appropriate ranges. If the pH value is less than 4.5, the sample is flagged for 30.01.

### 30.02. pH higher than expected

Every water-quality sample (environmental and QC) had its pH (p00400) checked for appropriate ranges. If the pH value is higher than 9, the sample is flagged for 30.02.

### 30.03. Dissolved Oxygen higher than expected

Every water-quality sample (environmental and QC) had its dissolved oxygen (p00300) checked for appropriate ranges. If the D.O. value is higher than 25, the sample is flagged for 30.03.

### 30.11. Greater than max value in old data

New Environmental samples are compared to "old" environmental samples at the same site from water-year 1996 and later, for pesticides, nutrients, field parameters, and major ions. When a parameter has 5 or more "old" values at the site, and a new sample has a new maximum for that parameter, that sample and parameter are flagged for 30.11. Pesticides in Schedule 2437 which existed in the earlier schedules 2033 or 2060 (under different pcodes) are compared to the older pcodes as well as the Schedule 2437 pcodes.

### 30.12. Less than min value in old data

New Environmental samples are compared to "old" environmental samples at the same site from water-year 1996 and later, for pesticides, nutrients, field parameters, and major ions. When a parameter has 5 or more "old" values at the site, and a new sample has a new minimum for that parameter, that sample and parameter are flagged for 30.12. Pesticides in Schedule 2437 which existed in the earlier schedules 2033 or 2060 (under different pcodes) are compared to the older pcodes as well as the Schedule 2437 pcodes.

### 30.13. Schedule 2437 Pesticide hits greater than max hits in old data

The number of Schedule 2437 pesticide hits in new environmental samples is compared to 2437 pesticide hits in "old" environmental samples at the same site from water-year 2013 and later (the year Schedule 2437 began). When a site has 5 or more "old" pesticide samples and a new sample has a new maximum number of pesticide hits, that sample is flagged for 30.13. Only pesticides in Schedule 2437 are counted as hits.

### 30.14 Less than minimum pesticide hits in old data

The number of Schedule 2437 pesticide hits in new environmental samples is compared to 2437 pesticide hits in "old" environmental samples at the same site from water-year 2013 and later (the year Schedule 2437 began). When a site has five or more "old" pesticide samples and a new sample has a new minimum number of pesticide hits, that sample is flagged for 30.14. Only pesticides in Schedule 2437 are counted as hits.

### 30.15. Greater than 99p in old data

New Environmental samples are compared to "old" environmental samples at the same site from water-year 1996 and later, for pesticides, nutrients, field parameters, and major ions. When a parameter has 24 or more "old" values at the site, and a new sample has a value greater than the 99th percentile of the "old" data set for that parameter, that sample and parameter are flagged for 30.15. Pesticides in Schedule 2437 which existed in the earlier schedules 2033 or 2060 (under different pcodes) are compared to the older pcodes as well as the Schedule 2437 pcodes.

### 30.16. Less than 1p in old data

New Environmental samples are compared to "old" environmental samples at the same site from water-year 1996 and later, for pesticides, nutrients, field parameters, and major ions. When a parameter has 24 or more "old" values at the site, and a new sample has a value less than the 1st percentile of the "old" data set for that parameter, that sample and parameter are flagged for 30.15. Pesticides in Schedule 2437 which existed in the earlier schedules 2033 or 2060 (under different pcodes) are compared to the older pcodes as well as the Schedule 2437 pcodes.

### 30.21. Questionable cation-anion balance

For each environmental sample, the cation-anion percent difference was be calculated (using the same formula, but a slightly simpler procedure, than what NWIS-QWDATA uses) and compared to the value for specific conductance (P00095). The cation-anion ballance is flagged for 30.21 if any of the following conditions is true 1. specific conductance is less than or equal to 100 microsiemens/cm, AND percent difference is greater (in absolute value) than +/- 15 percent 2. specific conductance is greater than 100 and less than or equal to 1000 microsiemens/cm, AND percent difference is greater (in absolute value) than +/- 10 percent 3. specific conductance is greater than 1000 microsiemens/cm, AND percent difference is greater (in absolute value) than +/- 5 percent

Suggested WQReview and QWDATA workflow for flipping DQI codes
=============================================================

WQReview
--------

1.  Use WQReview to pull data of interest.
2.  Review the Excel workbook tables and WQReview plots as part of your result review.
3.  Copy results that have been reviewed from the Excel "DQI needs review" tab to the "Ready for DQI change".
4.  Based on your review, add the new DQI code (R, Q, etc.) to the "DQI\_CD\_REVIEWED" column.
5.  Add any review comments to be added to NWIS in the "RESULT\_CM\_NWIS\_REVIEWED" column.
6.  Put the comment type (F for field or L for Lab) in the "RESULT\_CM\_TYPE\_REVIEWED" column.
7.  Add any additional comments to the "RESULT\_NOTES\_WQREVIEW" column. These will not be uploaded to NWIS, they will only be saved when the Excel workbook is saved.
8.  Under the WQReview "Generate batch DQI flip files" header, hit the "Generate batch files" button. A qwsample and qwresult tab will be generated in the Excel workbook.
9.  Using Windows Explorer, create or navigate to a project folder in your mapped home directory on NWIS. Save the Excel workbook here.
10. Copy everything but the column titles from the qwsample and qwresult Excel workbook tabs and paste them into seperate files using a text editor (Notepad, Notepad++, Wordpad) and save them in the project folder as qwsample and qwresult files. Some Water Quality Specialists prefer the qwsample and qwresult file to have a username and date/time stamp appended to the file name. E.g. qwsample\_tmills\_20181001

QWDATA - Consult with your Water Quality Specialist, Local Data Manager, and/or Data Base Administrator about your WSC practices for batch file changes in QWDATA.
------------------------------------------------------------------------------------------------------------------------------------------------------------------
