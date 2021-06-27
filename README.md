## Heterogeneity of desired speed in crowd dynamics

Clone the repository using the following command:

    git clone https://github.com/DaVinkie/ComplexSystems.git

After having cloned the repository, you need to collect the submodule **cromosim**.
This is done by running the following command:

    git submodule update --init --recursive

The repository assumes you're using SSH to identify yourself and will throw a key-error when you are not. If you use the standard password for Git, go to the file **.git/config** and change the URL underneath submodules (*line 20*) to:

    https://github.com/DaVinkie/cromosim.git

Afterwards run the previous command again.

In *cromosim/requirements.txt* you will find the required packages to run cromosim. These are relatively standard packages, but it is important to have update to the latest version of numpy.

To run a model simulation there are two options: 

1. Run a single simulation:

The **simulation\_demo.json** contains the parameters of the model. Adjust these to your liking. Of note are the ***background*** under domain and velocity distribution underneath ***groups*** and ***new\_groups***. Full documentation on the parameters can be found in the **`simulation_demo.py`**

To run a simulation, run the following command from the terminal:
	

    python model_simulation.py --json model_simulation.json


2. Run our experiments:

To replicate our experiments, run the following commands - without the json flag:
	

    python run_simulation_a.py
    python run_simulation_b.py

(Results from the experiment are already stored within the **results** folder. The complete runtime is about ***380 CPU hours***. The `simulation_a_loop.txt` / `simulation_b_loop.txt` files allow for running parallel simulation on one machine by keeping track of simulations that's currently being executed.)


Data handling is done in R (due to it's superior features for analyzing and working with data :):
The **analysis** folder contains the folowing scripts:

*analyze_a.R
analyze_b.R*

each of which is designated for collecting, cleaning, visializing, and analyzing results from the respective simulations. The analysis scripts can be run interactively to produce all required figures.

The **`generate_sims.R`** creates settings file for running the simulations and the **`results_animation.R`** file joins output from the cromosim with a calculated thoughput for creating a joint animation of the simulation and the increasing throughput.

![Simulation and its throughput](https://github.com/DaVinkie/ComplexSystems/blob/master/analysis/giffed.gif)
