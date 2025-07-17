
# Initialisation of IFN for medfateland

## What the workflow does?

+ Creates `sf` objects for package **medfateland** for each province and forest inventory survey (IFN2, IFN3 and IFN4).
+ Tests the resulting objects using one day simulation

## Where it can be run?
+ The workflow has been developed for Miquel's **laptop**, but can be run at **EMF server**, as long as source `emf_dataset_path` is updated in `_targets.R`.

## Data dependencies

| Data source       | Data location    | Previous pipeline |
|-------------------|------------------|-------------------|
| Harmonized Spanish National Forest Inventory (NFI) |`[emf_dataset_path]/ForestInventories/IFN_forestables/`| *emf_ifn_forestables* |
| Exact IFN coordinates |       `[emf_dataset_path]/ForestInventories/IFN_coordinates/`          | |
| PNOA MDT 25 m     |            `[emf_dataset_path]/Topography/Spain/PNOA_MDT25_PROVINCES_ETRS89/`    | |
| SoilGrids 2.0     |  `[emf_dataset_path]/Soils/Global/SoilGrids/`  | |
| Soil depth data from Shangguan et al. (2017) | `[emf_dataset_path]/Soils/Global/SoilDepth_Shangguan2017/` | |


## EMF R packages dependencies

|  R package  |   Functionality provided  |
|-------------|------------------|
| **medfate** | Simulation control parameters |
| **medfateland** | Landscape initialisation routines, simulation routines for testing |
| **traits4models** | Dataset `SpParamsES` |

## Outputs

+ `sf` objects ready for **medfateland** simulations for each province and IFN, including or not a correction of rock fragment content.
