# Cost & GHG Manager v0.1

A browser-based application for Activity-Based Standard Cost & GHG Accounting in Multi-Level Manufacturing Structures.

The application supports manufacturing master data, product structures (BOM), production routings, production orders, activity responses, standard and actual cost and GHG accounting, and analytical variance analysis.

Developed as part of a PhD research implementation and for teaching purposes at TU Wien.

## Getting Started

The application runs locally in a web browser. No separate installation of R, RStudio, Python, Node.js, or Go is required.

Download and extract the complete application folder before starting.

**Windows:** Double-click `START-WINDOWS.bat`.

**macOS:** Double-click `START-MAC.command`.

The application opens in the default browser. If it does not open automatically, use the local address displayed in the server window.

Keep the server running while using the application. Save your project before closing it.

*Note: Native macOS startup has not yet been verified. macOS may display security warnings because the launcher is not signed or notarized. Follow your institution's guidance for trusted software without disabling system protections.*

## Included SCM Examples

Two manufacturing cases are provided:

| Case | Master Data | Completed Project |
|---|---|---|
| Small | `examples/SCM_MasterData_Small/` | `examples/AIS_SCM_Small-DT_Project.json` |
| Full | `examples/SCM_MasterData_Full_Updated/` | `examples/AIS_SCM_Full_Project_Updated.json` |

The **Small case** illustrates the basic manufacturing and accounting workflow.

The **Full case** contains a larger multi-level manufacturing structure, including shared intermediate products and repeated production-node occurrences.

Each case includes eight source CSV tables and a saved project containing completed production orders.

## Working with the Examples

**Load an existing project**

Select **Load Project** and choose one of the supplied JSON files. The project can then be explored through the Dashboard, Production Information, Production Execution, Standard Accounting, and Variance Analysis modules.

**Create a new project**

Select **New Project** and import the CSV files from either SCM example folder.

Import the six master data tables first:

`MAT.TBL`, `WIP.ECSI`, `FP.ECSI`, `ACTY.ECSI`, `EQIP.TBL`, `ENY.TBL`.

Then import `BOM.ECSI` and `Routing.ECSI`.

After importing the manufacturing data, create a production order, generate its production node tree (`ProNodeTree`), execute production, and enter actual observations through **Edit Activity Response**.

The accounting and variance results can then be calculated in the corresponding modules.

Use **Save Project** to export your work as a JSON file.

For definitions of entities, attributes, and accounting measures, see the application's **Help** section.

## Software and Research Use

The application includes the R and JavaScript implementation, together with the required webR runtime and supporting packages.

The included SCM datasets are intended for demonstrating Activity-Based Standard Cost & GHG Accounting in multi-level manufacturing structures.

Third-party software retains its respective license notices in the distribution.

## Citation

For academic use, please cite the archived version of this application using the DOI provided in the associated Zenodo record.

[https://doi.org/10.5281/zenodo.23769870](https://doi.org/10.5281/zenodo.23769870)
