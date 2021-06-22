from cromosim import *
from cromosim.micro import *
import csv
import numpy as np

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

	people[dom_name]["xyrv"] = remove_overlaps_in_box(dom, [dom.xmin, dom.xmax, dom.ymin, dom.ymax],
                                    people[dom_name]["xyrv"], people[dom_name]["destinations"], dt, people[dom_name]["rng"],
                                    dmin_people=dmin_people,
                                    dmin_walls=dmin_walls,
                                    projection_method=projection_method, verbose=False)

	for pth in new_people["paths"]:
		people[dom_name]["paths"][pth] = new_people["paths"][pth]

	people[dom_name]["last_id"] = dom_name+'_'+str(people[dom_name]["xyrv"].shape[0]-1)

	return people

def export_data(sensors, output_dir, file_name):
    """
	When function is called, save collected data as csv.

	Parameters::
	sensors: senosors
        sensors object containing the data to be exported
	output_dir: 
        location of the file to written
    file_name:
        name of the file
	"""
    for s in range(len(sensors["room"])):
        with open(output_dir + "/" + file_name + "_sensor_" + str(s) + '.csv', 'w', encoding='UTF8', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(["Time"])
        
            time_list = sensors["room"][s]["times"]
        
            for i in range(len(time_list)):
                writer.writerow([time_list[i]])
    
    
def column(matrix, i):
    return [row[i] for row in matrix]

def people_at_spawn(people):
    return(sum(x < 3 for x in column(people["xyrv"], 0)))

def slowdown_velocity(dom, people, slowed_people, nn=3, seed=0, slowdown=0.1, duration=5):
	"""
	At certain time points, a random selection of agents get their desired velocity slowed
	down.
	
	"""
	dom_name = dom.name
	n_people = people[dom_name]["xyrv"].shape[0]

	if (seed > 0):
		rng = np.random.RandomState(seed)
	else:
		rng = np.random.RandomState()

	# Select a random group from agents in model
	vd_selection = []
	selection = rng.randint(0, n_people, nn)
	selection = np.unique(selection)

	# Keep track of separate list to prevent agents from being doubly slowed down.
	# Instead, the duration is reset.
	for s in selection:
		if str(s) not in list(slowed_people):
			vd_selection.append(s)

	print("Slowing down: ", vd_selection)

	# Slow down agents their desired velocity
	# Vd = people[dom_name]["Vd"]
	# Vd[vd_selection] = Vd[vd_selection]*slowdown
	# people[dom_name]["Vd"] = Vd

	# U = people[dom_name]["U"]
	# U[vd_selection] = U[vd_selection]*slowdown
	# people[dom_name]["U"] = U

	# Vd = people[dom_name]["Vd"]
	# Vd[vd_selection] = np.array([0.0, 0.0])
	# people[dom_name]["Vd"] = Vd

	# U = people[dom_name]["U"]
	# U[vd_selection] = np.array([0.0, 0.0])
	# people[dom_name]["U"] = U

	v_calc = people[dom_name]["xyrv"][vd_selection, 3] * slowdown

	people[dom_name]["xyrv"][vd_selection, 3] = v_calc


	for p in selection:
		des_vd = people[dom_name]["Vd"][p]
		slowed_people[str(p)] = [duration, des_vd]

	return people, slowed_people

def adjust_velocity(dom, people, slowed_people, dt=0.005, slowdown=0.1):
	"""

	"""
	dom_name = dom.name
	print("slowed_people: ", slowed_people)
	for sp in slowed_people.copy():
		print(slowed_people[sp])
		print('Vd: ', sp, people[dom_name]["Vd"][int(sp)])
		if slowed_people[sp][0] <= 0.0:
			people[dom_name]["xyrv"][int(sp), 3] = people[dom_name]["xyrv"][int(sp), 3] * (1/slowdown)
			print("REMOVING: ", sp)
			del(slowed_people[sp])
		else:
			slowed_people[sp][0] -= dt 

	return people, slowed_people