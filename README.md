# desdt

Discrete Event Simulation (DES) using data.table in R

## Goal

To compare DES for health economics between the `data.table` and `simmer` packages. It has been claimed that data.table is far more efficient.

## Limitations

`simmer` is a full featured discrete event simulation package in R. Our Health Policy group has been using it to simulate patient trajectories through treatment options and evaluating outcomes. Our needs do not include resource constraints (access to health care, etc) as part of research domain. As such, we do not utilize one of the main purposes of DES in our research. A researcher has put forth that `data.table` is more than sufficient and reaps large gains in the efficiency of simulations. Thus we need to compare these two technologies when limited to cases where treatment resources are assumed infinite.

## Case Study Description

For demonstration purposes we have chosen the Framingham CVD model coupled with a secular death model, and will try to keep the core model code as similar as possible. 

## Alternatives

It has been suggested to also include a pre-allocated list as an alternative method to recording simulation results.

   vector('list', 1e8)
   data.table::set


