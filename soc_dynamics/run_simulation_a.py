# Authors:
#     Sylvain Faure <sylvain.faure@universite-paris-saclay.fr>
#     Bertrand Maury <bertrand.maury@universite-paris-saclay.fr>
#
#      cromosim/examples/micro/social/micro_social.py
#      python micro_social.py --json input.json
#
# License: GPL


import sys, os
from cromosim import *
from cromosim.micro import *
from optparse import OptionParser
import json
import pandas


# Extra necessary imports
from model_expansion import *
import csv


# load the conditions settings file 
settings_file = "../Maps_with_obstacles/Corridor_unidirectional/settings_a.json"
settings = pandas.read_csv('settings_a.csv')
with open("simulation_a_loop.txt") as f:
    loop = int(f.read())
        
# load the map settings
parser = OptionParser(usage="usage: %prog [options] filename", version="%prog 1.0")
parser.add_option('--json',dest="jsonfilename",default=settings_file, type="string", action="store",help="Input json filename")
opt, remainder = parser.parse_args()
print("===> JSON filename = ",opt.jsonfilename)
with open(opt.jsonfilename) as json_file:
    try:
        input = json.load(json_file)
    except json.JSONDecodeError as msg:
        print(msg)
        print("Failed to load json file ",opt.jsonfilename)
        print("Check its content \
            (https://fr.wikipedia.org/wiki/JavaScript_Object_Notation)")
        sys.exit()


def column(matrix, i):
    return [row[i] for row in matrix]

    
# run the simulation
while loop < len(settings):
    
    # the loop tracker
    with open("simulation_a_loop.txt") as f:
        loop = int(f.read()) + 1 
    
    with open("simulation_a_loop.txt", "w") as f:
        f.write(str(loop))
    print(loop)
    
    # data settings
    output_dir    = "../results/" + "a_stop/" + str(settings["condition"][loop])
    map_condition = "../Maps_with_obstacles/Corridor_unidirectional/a" + str(settings["condition"][loop]) + ".png"
    seed          = settings["seed"][loop]
    new_std       = settings["std"][loop]

    # update std
    for i in range(len(input["people_init"][0]["groups"])):
        input["people_init"][0]["groups"][i]["velocity_distribution"][2] = new_std
    for i in range(len(input["new_groups"])):
        input["new_groups"][i]["velocity_distribution"][2] = new_std
    
    with_graphes = False
    json_domains = input["domains"]
    #print("===> JSON data used to build the domains : ",json_domains)
    json_people_init = input["people_init"]
    #print("===> JSON data used to create the groups : ",json_people_init)
    json_sensors = input["sensors"]
    #print("===> JSON data used to create sensors : ",json_sensors)
    Tf = input["Tf"]
    dt = input["dt"]
    drawper = input["drawper"]
    mass = input["mass"]
    tau = input["tau"]
    F = input["F"]
    kappa = input["kappa"]
    delta = input["delta"]
    Fwall = input["Fwall"]
    lambda_ = input["lambda"]
    eta = input["eta"]
    projection_method = input["projection_method"]
    dmax = input["dmax"]
    dmin_people = input["dmin_people"]
    dmin_walls = input["dmin_walls"]
    plot_p = input["plot_people"]
    plot_c = input["plot_contacts"]
    plot_v = input["plot_velocities"]
    plot_vd = input["plot_desired_velocities"]
    plot_pa = input["plot_paths"]
    plot_s  = input["plot_sensors"]
    plot_pa = input["plot_paths"]
    prefix  = output_dir
    print("===> Final time, Tf = ",Tf)
    print("===> Time step, dt = ",dt)
    print("===> To draw the results each drawper iterations, \ drawper = ",drawper)
    print("===> Maximal distance to find neighbors, dmax = ", dmax,", example : 2*dt")
    print("===> ONLY used during initialization ! Minimal distance between \ persons, dmin_people = ",dmin_people)
    print("===> ONLY used during initialization ! Minimal distance between a \ person and a wall, dmin_walls = ",dmin_walls)
    
    ### ADDED BY US: ##################################
    if input["addper"]:
        add_per = input["addper"]
    else: 
        add_per = None
    
    # People randomly slowing down
    if input["slowdown"]:
        slowdown = input["slowdown"]
        slowed_people = {}
    else:
        slowdown = None
    
    if input["sd_period"]:
        duration = input["sd_period"]
    else:
        duration = None
    
    if input["n_slowdown"]:
        n_slowdown = input["n_slowdown"]
    else:
        n_slowdown = None
    
    if input["slow_per"]:
        slow_per = input["slow_per"]
    else:
        slow_per = None
    ###################################################
    
    """
        Build the Domain objects
    """
    domains = {}
    for i,jdom in enumerate(json_domains):
        jname = jdom["name"]
        print("===> Build domain number ",i," : ",jname)
        jbg = map_condition
        jpx = jdom["px"]
        jwidth = jdom["width"]
        jheight = jdom["height"]
        jwall_colors = jdom["wall_colors"]
        if (jbg==""):
            dom = Domain(name=jname, pixel_size=jpx, width=jwidth,
                         height=jheight, wall_colors=jwall_colors)
        else:
            dom = Domain(name=jname, background=jbg, pixel_size=jpx,
                         wall_colors=jwall_colors)
        ## To add lines : Line2D(xdata, ydata, linewidth)
        for sl in jdom["shape_lines"]:
            line = Line2D(sl["xx"],sl["yy"],linewidth=sl["linewidth"])
            dom.add_shape(line,outline_color=sl["outline_color"],
                          fill_color=sl["fill_color"])
        ## To add circles : Circle( (center_x,center_y), radius )
        for sc in jdom["shape_circles"]:
            circle = Circle( (sc["center_x"], sc["center_y"]), sc["radius"] )
            dom.add_shape(circle,outline_color=sc["outline_color"],
                          fill_color=sc["fill_color"])
        ## To add ellipses : Ellipse( (center_x,center_y), width, height,
        ##                            angle_in_degrees_anti-clockwise )
        for se in jdom["shape_ellipses"]:
            ellipse = Ellipse( (se["center_x"], se["center_y"]),
                                se["width"], se["height"],
                                se["angle_in_degrees_anti-clockwise"])
            dom.add_shape(ellipse,outline_color=se["outline_color"],
                          fill_color=se["fill_color"])
        ## To add rectangles : Rectangle( (bottom_left_x,bottom_left_y),
        ##                       width, height, angle_in_degrees_anti-clockwise )
        for sr in jdom["shape_rectangles"]:
            rectangle = Rectangle( (sr["bottom_left_x"],sr["bottom_left_y"]),
                                   sr["width"], sr["height"],
                                   sr["angle_in_degrees_anti-clockwise"])
            dom.add_shape(rectangle,outline_color=sr["outline_color"],
                          fill_color=sr["fill_color"])
        ## To add polygons : Polygon( [[x0,y0],[x1,y1],...] )
        for spo in jdom["shape_polygons"]:
            polygon = Polygon(spo["xy"])
            dom.add_shape(polygon,outline_color=spo["outline_color"],
                          fill_color=spo["fill_color"])
        ## To build the domain : background + shapes
        dom.build_domain()
        ## To add all the available destinations
        for j,dd in enumerate(jdom["destinations"]):
            desired_velocity_from_color=[]
            for gg in dd["desired_velocity_from_color"]:
                desired_velocity_from_color.append(
                    np.concatenate((gg["color"],gg["desired_velocity"])))
            dest = Destination(name=dd["name"],colors=dd["colors"],
            excluded_colors=dd["excluded_colors"],
            desired_velocity_from_color=desired_velocity_from_color,
            velocity_scale=dd["velocity_scale"],
            next_destination=dd["next_destination"],
            next_domain=dd["next_domain"],
            next_transit_box=dd["next_transit_box"])
            print("===> Destination : ",dest)
            dom.add_destination(dest)
            if (with_graphes):
                dom.plot_desired_velocity(dd["name"],id=100*i+10+j,step=20)
    
        print("===> Domain : ",dom)
        if (with_graphes):
            dom.plot(id=100*i)
            dom.plot_wall_dist(id=100*i+1,step=20)
    
        domains[dom.name] = dom
    
    print("===> All domains = ",domains)
    
    """
        To create the sensors to measure the pedestrian flows
    """
    
    all_sensors = {}
    for domain_name in domains:
        all_sensors[domain_name] = []
    for s in json_sensors:
        s["id"] = []
        s["times"] = []
        s["xy"] = []
        s["dir"] = []
        all_sensors[s["domain"]].append(s)
        #print("===> All sensors = ",all_sensors)
    
    """
        Initialization
    """
    
    ## Current time
    t = 0.0
    counter = 0
    
    ## Initialize people
    all_people = {}
    for i,peopledom in enumerate(json_people_init):
        dom = domains[peopledom["domain"]]
        groups = peopledom["groups"]
        print("===> Group number ",i,", domain = ",peopledom["domain"])
        people = people_initialization(dom, groups, dt,
            dmin_people=dmin_people, dmin_walls=dmin_walls, seed=seed,
            itermax=10, projection_method=projection_method, verbose=False)
        I, J, Vd = dom.people_desired_velocity(people["xyrv"],
            people["destinations"])
        people["Vd"] = Vd
        for ip,pid in enumerate(people["id"]):
            people["paths"][pid] = people["xyrv"][ip,:2]
        contacts = None
        if (with_graphes):
            colors = people["xyrv"][:,2]
            plot_people(100*i+20, dom, people, contacts, colors, time=t,
                        plot_people=plot_p, plot_contacts=plot_c,
                        plot_velocities=plot_v, plot_desired_velocities=plot_vd,
                        plot_sensors=plot_s, sensors=all_sensors[dom.name],
                        savefig=True, filename=prefix+dom.name+'_fig_'+ \
                        str(counter).zfill(6)+'.png')
        all_people[peopledom["domain"]] = people
    
    # print("===> All people = ",all_people)
    
    """
        Main loop
    """
    
    cc = 0
    draw = True
    
    ### ADDED BY US: ##################################
    adding = False
    slowing = False
    ###################################################
    
    while (t<Tf):
    
        print("\n===> Time = "+str(t))
    
        ## Compute people desired velocity
        for idom,name in enumerate(domains):
            print("===> Compute desired velocity for domain ",name)
            dom = domains[name]
            people = all_people[name]
            I, J, Vd = dom.people_desired_velocity(people["xyrv"],
                people["destinations"])
            people["Vd"] = Vd
            people["I"] = I
            people["J"] = J
    
        ## Look at if there are people in the transit boxes
        print("===> Find people who have to be duplicated")
        virtual_people = find_duplicate_people(all_people, domains)
        #print("     virtual_people : ",virtual_people)
    
        ## Social forces
        for idom,name in enumerate(domains):
            print("===> Compute social forces for domain ",name)
            dom = domains[name]
            people = all_people[name]
    
            try:
                xyrv = np.concatenate((people["xyrv"],
                    virtual_people[name]["xyrv"]))
                Vd = np.concatenate((people["Vd"],
                    virtual_people[name]["Vd"]))
                Uold = np.concatenate((people["Uold"],
                    virtual_people[name]["Uold"]))
            except:
                xyrv = people["xyrv"]
                Vd = people["Vd"]
                Uold = people["Uold"]
    
            if (xyrv.shape[0]>0):
    
                if (np.unique(xyrv, axis=0).shape[0] != xyrv.shape[0]):
                    print("===> ERROR : There are two identical lines in the")
                    print("             array xyrv used to determine the \
                        contacts between")
                    print("             individuals and this is not normal.")
                    sys.exit()
    
                contacts = compute_contacts(dom, xyrv, dmax)
                print("     Number of contacts: ",contacts.shape[0])
                Forces = compute_forces( F, Fwall, xyrv, contacts, Uold, Vd,
                                         lambda_, delta, kappa, eta)
                nn = people["xyrv"].shape[0]
                all_people[name]["U"] = dt*(Vd[:nn,:]-Uold[:nn,:])/tau + \
                              Uold[:nn,:] + \
                              dt*Forces[:nn,:]/mass
                ## only for the plot of virtual people :
                virtual_people[name]["U"] = dt*(Vd[nn:,:]-Uold[nn:,:])/tau + \
                              Uold[nn:,:] + \
                              dt*Forces[nn:,:]/mass
    
    
                all_people[name], all_sensors[name] = move_people(t, dt,
                                               all_people[name],
                                               all_sensors[name])
    
            if (draw and with_graphes):
                ## coloring people according to their radius
                colors =  all_people[name]["xyrv"][:,2]
                ## coloring people according to their destinations
                # colors = np.zeros(all_people[name]["xyrv"].shape[0])
                # for i,dest_name in enumerate(all_people[name]["destinations"]):
                #     ind = np.where(all_people[name]["destinations"]==dest_name)[0]
                #     colors[ind]=i
                plot_people(100*idom+20, dom, all_people[name], contacts,
                            colors, virtual_people=virtual_people[name], time=t,
                            plot_people=plot_p, plot_contacts=plot_c,
                            plot_paths=plot_pa, plot_velocities=plot_v,
                            plot_desired_velocities=plot_vd, plot_sensors=plot_s,
                            sensors=all_sensors[dom.name], savefig=True,
                            filename=prefix+dom.name+'_fig_'
                            + str(counter).zfill(6)+'.png')
                plt.pause(0.01)
    
        ## Update people destinations
        all_people = people_update_destination(all_people,domains,dom.pixel_size)
    
        ## Update previous velocities
        for idom,name in enumerate(domains):
            all_people[name]["Uold"] = all_people[name]["U"]
    
        ## Print the number of persons for each domain
        for idom,name in enumerate(domains):
            print("===> Domain ",name," nb of persons = ",
                all_people[name]["xyrv"].shape[0])
    
    ### ADDED BY US: ######################################
        # Temporal addition:
        if(people_at_spawn_a(all_people[name]) < 20):
            if adding:
                seed += 1
                all_people = add_people(input, dom, all_people, seed)
    
    
        if t >= add_per and (t % add_per) <= dt:
            adding = True
        else:
            adding = False
    
        # People randomly slowing down
        if slowing:
            all_people, slowed_people = adjust_velocity(dom, all_people, slowed_people, dt, slowdown)
            all_people, slowed_people = slowdown_velocity(dom, all_people, slowed_people, n_slowdown, seed, slowdown, duration)
            print(">>> SLOWING DOWN THE FOLLOWING PEOPLE: ", slowed_people)
        else: 
            all_people, slowed_people = adjust_velocity(dom, all_people, slowed_people, dt, slowdown)
    
        if t >= slow_per and (t % slow_per) <= dt:
            slowing = True
        else:
            slowing = False


        t += dt
        cc += 1
        counter += 1
        if (cc>=drawper):
            draw = True
            cc = 0
        else:
            draw = False
    #######################################################
        
        
    ### ADDED BY US: ######################################
    # save data at the end of the trial
    export_data(all_sensors[name], output_dir, str(loop))
    #######################################################

