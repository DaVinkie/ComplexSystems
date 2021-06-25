After having cloned the repository, you need to collect the submodule 'cromosim'.
This is done by running the following command:
	git submodule update --init --recursive

The repository assumes you're using SSH to identify yourself and will throw a key-error when 
you are not. If you use the standard password for Git, go to the file '.git/config' and change 
the URL underneath submodules to:
	https://github.com/DaVinkie/cromosim.git

Afterwards run the previous command again.

In cromosim/requirements.txt you will find the required packages to run cromosim. These are
relatively standard packages, but it is important to have update to the latest version of 
Numpy.


To run a model simulation there are two options:
1. Run a single simulation:

The simulation\_demo.json contains the parameters of the model. Adjust these to your liking.
Of note are the "background" under domain and velocity distribution underneath "groups" and
"new\_groups". Full documentation on the parameters can be found in the 'simulation\_demo.py'.

To run a simulation, run the following command from the terminal:
	python model_simulation.py --json model_simulation.json


2. Run our experiments:

To replicate our experiments, run the following commands - without the json flag:
	python run_simulation_a.py
	python run_simulation_b.py

Data handling is done in R:
 
