from cromosim import *
from cromosim.micro import *

def add_people(input, dom, people, seed):
	"""
	When function is called, add agents to the model.

	Parameters::
	input: json input
		JSON input file that holds configuration details of the to-be-added agents.
	dom: Domain
		Domain the agents are added to. Contains domain information. Not retrieved from input file
		because our simulations do not cover multiple domains.
	people: dictionary
		Dictionary of agents in the model containing their coordinates, velocities, destination and 
	"""
	# Parameter retrieval from input file
	target_group = input["new_groups"]
	dt = input["dt"]
	dmin_people = input["dmin_people"]
	dmin_walls = input["dmin_walls"]
	# seed = input["seed"]
	projection_method = input["projection_method"]
	dom_name = dom.name

	print(projection_method)

	# Initialize a new group of agents
	new_people = people_initialization(dom, target_group, dt, dmin_people=dmin_people, 
					dmin_walls=dmin_walls, seed=seed, itermax=10, projection_method=projection_method,
					verbose=True)
	I_n, J_n, Vd_n = dom.people_desired_velocity(new_people["xyrv"], new_people["destinations"])
	new_people["Vd"] = Vd_n
	new_people["I"] = I_n
	new_people["J"] = J_n

	# Adjust the IDs of the created agents
	last_id = people[dom_name]["xyrv"].shape[0]
	new_people["id"] = np.char.add([dom.name+'_']*new_people["xyrv"].shape[0], 
		(np.arange(new_people["xyrv"].shape[0])+last_id).astype('<U3'))

	# Generate path starting points for created agents
	for ip,pid in enumerate(new_people["id"]):
		new_people["paths"][pid] = new_people["xyrv"][ip,:2]
   
	# These variables have a different data structure than arrays and can't be regularly appended.
	skip_var = ['paths', 'rng', 'last_id']
	keys = list(people[dom_name].keys())
	for sv in skip_var:
		keys.remove(sv)
	
	# Append new agent data to existing model data.
	for key in keys:
		old = people[dom_name][key]
		add = new_people[key]
		new_arr = np.append(old, add, axis=0)
		people[dom_name][key] = new_arr

	for pth in new_people["paths"]:
		people[dom_name]["paths"][pth] = new_people["paths"][pth]

	people[dom_name]["last_id"] = dom_name+'_'+str(people[dom_name]["xyrv"].shape[0]-1)

	return people
