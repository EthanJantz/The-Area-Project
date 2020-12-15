
import os
file_directory = os.getcwd()

def root_run(file_directory):
	
	pathway_list = []
	for root, dirs, csv_files in os.walk("%s"%(file_directory)):
	    for csv_file in csv_files:
	        if csv_file.endswith(".txt"):
	             #print(os.path.join(root, csv_file))
	             active_csv_file = csv_file
	            # print "hi"
	             pathway_list.append(active_csv_file)
	             print(active_csv_file) 
	             print(sorted(pathway_list))
	             print("show")
	return sorted(pathway_list)
             # active_csv_file is file path 
             # thi s fucniton will create abd returns a list of file names that will populate the read in file funciton.
             # items must be called in via the index thoruhg some loop 


### THINKK THROUHG CONCEPTUALIZAITON TO ITERATE THROUHG FILES LISTED W/O META DATA TO APPEND TO 3 GIANT CSV FILE FOR EACH SIMULAITON---MIGHT TAKE FODLERS OUT AND RUN PYTHON ON THIEM SPERATE 
###  FOR FILE IN FILE : 
		# CURRENT_ACTIVE FILE = read_in_file( FILE):
		 # UPDATED_OUTPUT_FILE = write_info_to_file(CUREENT_ACTIVE_FILE) --- THIS SHOUDL APPEND TO FILE---- TICKS

def meta_root_run(file_directory):
	pathway_list = []
	for root, dirs, meta_files in os.walk("%s"%(file_directory)):
		for meta_file in meta_files:
			if meta_file.endswith(".csv") and meta_file.startswith("meta"):
				print(os.path.join(root,meta_file))
				active_meta_file = meta_file
				print("meta hi")
				pathway_list.append(active_meta_file)
				print(active_meta_file)
				print(sorted(pathway_list))
		return sorted(pathway_list)


def read_in_file(file): # cleans and import text files into python usable data types  
	python_list = []
	pylist2= []
	cnt = 0
	cnt2 = 0
	with open(file,"r") as open_text:
		for content in open_text:
			new_line = content.split(" ")
			print("First Loop Successful")
			#print new_line
		for s_lines in new_line[5:]:
			list_strings = s_lines.split(",")

			cnt2 += 1
			#XXXXXXXXXXXXXX

			#KEY ISSUES THAT THIS CODE MUST INCORPORATE PARAMTERS(meta data) AS A WAY TO ORGANIZE THEM  SUCH  MODEL TITLE
			#MUST HAVE PARAMETERS AND MODEL TITLE 

			#XXXXXXXXXXXXXXX
			ticks = int(list_strings[0][1:])
			mortgage_buyout_ratio = float(list_strings[1])
			soc_quality = float(list_strings[2])
			monetary_valuation = float(list_strings[3])
			non_normalized_norfolk_offer = float(list_strings[4])
			success_flag = int(list_strings[5])
			success_total = int(list_strings[6])
			houses = int(list_strings[7])
			houses_network_4 = int(list_strings[9])
			print(houses_network_4)
			houes_network_3 = int(list_strings[10])
			houes_network_2 = int(list_strings[11]) #index number might be off
			Total_Links = int(list_strings[-7])
			hold_out_ratio = float(list_strings[-6])
			initial_value = int(list_strings[-5])
			neigh_iso = str(list_strings[-4])
			link_iso = str(list_strings[-3]) 

			#print "links"
			#print Total_Links
			#print link_iso
			#print neigh_iso
			python_list.append([ticks,mortgage_buyout_ratio,soc_quality,monetary_valuation,non_normalized_norfolk_offer, success_flag,success_total,houses,houses_network_4,houes_network_3,houes_network_2,Total_Links,hold_out_ratio,initial_value,neigh_iso,link_iso])
			cnt += 1
			#print "Here"
			#print python_list[0]
			#return 
	return python_list


def readin_meta_data(meta_file):
	meta_list = []
	with open(meta_file,'r') as active_meta_file:
		for content in active_meta_file:
			new_line = content.split(",") #next interation use text block for if "Parameter title" = variable
			print("meta loop succesful")
			print(new_line)
			blocksize = int(new_line[1][0:]) 
			resident_strategy = str(new_line[3][0:])
			social_affinity = int(new_line[5][0:])
			social_type = str(new_line[7][0:])
			residential_density = int(new_line[9][0:])
			negative_impact = int(new_line[11][0:])
			impact_effects = str(new_line[13][0:])
			network_pref = str(new_line[15][0:]) # 17 redudnacy 
			distribution = str(new_line[19][0:])
			offer_adjustment = int(new_line[21][0:])
			#social_type2 = str(new_line[23][0:]) # 23 redudancy 
			gamma_mean = int(new_line[25][0:]) #formerly mid mortgage 
			gamma_std = int(new_line[27][0:-1]) #formel low_mortgage
			#print offer_adjustment 
			print (blocksize, resident_strategy, social_affinity,residential_density, negative_impact, impact_effects, network_pref, distribution, offer_adjustment,social_type,gamma_mean, gamma_std)
			meta_list.append([blocksize, resident_strategy, social_affinity, residential_density, negative_impact,impact_effects, network_pref, distribution, offer_adjustment,social_type,gamma_mean, gamma_std])
			return meta_list




def write_info_to_file(s,p_list,meta_list): # takes new clean python data and organizes it into csv
# can add meta-data clean python data to this writing funciton with new list variable at funciton input
	cnt = 0
	with open (s,"a") as f:
		for info in p_list:
			print(info) 
			blocksize = int(meta_list[0][0])
			resident_strategy = str(meta_list[0][1])
			social_affinity = int(meta_list[0][2])
			
			residential_density = int(meta_list[0][3])
			negative_impact = int(meta_list[0][4])
			
			impact_effects = str(meta_list[0][5])
			network_pref = str(meta_list[0][6])
			distribution = str(meta_list[0][7])
			offer_adjustment =int(meta_list[0][8])
			social_type = str(meta_list[0][9])
			gamma_mean = int(meta_list[0][10])
			gamma_std = int(meta_list[0][11])
			#print("look here")
			#print Norfolk_Strategy
			#print residential_density
			#print impact_effects
			#print negative_impact
			#print network_pref
			#print distribution
			#########

			ticks = int(info[0])
			mortgage_buyout_ratio = float(info[1])
			soc_quality = int(info[2])
			monetary_valuation = float(info[3])
			non_normalized_norfolk_offer = float(info[4])
			success_flag = str(info[5])
			success_total = int(info[6])
			houses = int(info[7])
			houses_network_4 = int(info[8])
			houses_network_3 = int(info[9])
			houses_network_2 = int(info[10])
			Total_Links = int(info[9])
			hold_out_ratio = info[12]
			initial_value = int(info[13]) # is this the correct value?
			neigh_iso = str(info[14])
			link_iso = str(info[15])
			print("hhh")
			print(monetary_valuation,non_normalized_norfolk_offer,success_flag)
			print(blocksize, resident_strategy, social_affinity,residential_density,negative_impact,gamma_mean,gamma_std,ticks,mortgage_buyout_ratio,soc_quality,monetary_valuation,non_normalized_norfolk_offer,success_flag,success_total,houses,houses_network_4,houses_network_3,houses_network_2,Total_Links,impact_effects,network_pref,distribution,offer_adjustment,hold_out_ratio,initial_value,neigh_iso,link_iso,social_type)
			print("hhh")
			f.write("%i,%s,%i,%i,%i,%i,%i,%i,%f,%i,%f,%f,%s,%i,%i,x%i,%i,%i,%i,%s,%s,%s,%i,%i,%i,%s,%s,%s\n"%(blocksize, resident_strategy, social_affinity,residential_density,negative_impact,gamma_mean,gamma_std,ticks,mortgage_buyout_ratio,soc_quality,monetary_valuation,non_normalized_norfolk_offer,success_flag,success_total,houses,houses_network_4,houses_network_3,houses_network_2,Total_Links,impact_effects,network_pref,distribution,offer_adjustment,hold_out_ratio,initial_value,neigh_iso,link_iso,social_type))
			

def call1():
	directory_cnt = 0
	# will have to make a loop for number of files in directory to read in file as " sample text" varaible, updated each loop via a counter in super lsit 
	u = root_run(file_directory) # create sorted list of all directory items saving static items as u 
	t = meta_root_run(file_directory) # sorted directory of  metafiles 
	for items in u :
		x =read_in_file(u[directory_cnt])  # this should a directory pulled from U- list by direcotry counter serving as index#
		#p = readin_meta_data(p[directory_cnt]) # this repalce fucniton below 
		p = readin_meta_data(t[directory_cnt])
		write_info_to_file("output.csv",x,p) # p should be in put in as list
		directory_cnt += 1  # this will count upward until outside of index range 
		if directory_cnt +1 == len(u)+1: # this ensures that the cnt goes up until the last item and ends 
			print("end")
			return
	#this produces a csv file with al rlevanat data sorted simulaiton group via the parameter settings  
	#write_info_to_file("name_must_change.txt", x)

call1()