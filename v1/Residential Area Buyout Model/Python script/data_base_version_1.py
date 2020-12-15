
import sqlite3

file_title = "Test"
def create_db_and_table():
    conn = sqlite3.connect("%smydatabase.db"%file_title)
    cursor = conn.cursor()
    sql = "CREATE TABLE file_title (Block_Size int, Resident_Strategy text, Social_Affinity int, Resident_Density int, Negative_Impact int, Ticks int, Mortgage_Buyout_Ratio int, Social_Quality int, Monetary_Valuation int, Non_Normalized_Offer int, Success int, Total_Success int, Houses int, TwoPlus_Network int, Total_Links int, Impact_effects text, Network_Pref text, Distribution text, Offer_Adjustment, Hold_out_ratio int, Initial_value int, Neigh_iso int, Link_iso int, ThreeH_Network int, FourH_Network int, Gamma_mean int, Gamma_std int, Social_Type text)"
    cursor.execute(sql)
    cursor.close()
# will produce an error on last row of the csv file if empty but recognized---data is all in tact through 
def add_simulation_data_to_database(input_file):
   
    conn = sqlite3.connect("%smydatabase.db"%file_title)
    cursor = conn.cursor()

    with open(input_file, "r") as f:
         for line in f:
             if not line.startswith("Artifact I don't feel like changing"):
                L = line.split(",")
                 # might need to add a count feature while loop that will close this out beofore and error 
                Block_Size =L[0]
                Resident_Strategy=L[1]
                Social_Affinity = L[2]
                Resident_Density =L[3]
                Negative_Impact= L[4]
                Gamma_mean = L[5]
                Gamma_std =L[6]
                Ticks = L[7]
                Mortgage_Buyout_Ratio = L[8]
                Social_Quality = L[9]
                Monetary_Valuation = L[10]
                Non_Normalized_Offer= L[11]
                Success = L[12]
                Total_Success = L[13]
                Houses = L[14]
                FourH_Network = L[15]
                ThreeH_Network =L[16]
                TwoPlus_Network = L[17]
                Total_Links = L[18]
                Impact_effects = L[19]
                Network_Pref = L[20]
                Distribution = L[21]
                Offer_Adjustment = L[22]
                Hold_out_ratio  =  L[23]
                Initial_value = L[24]
                Neigh_iso = L[25]
                Link_iso = L[26]
                Social_Type = L[27]

                sql = "INSERT INTO file_title (Block_Size, Resident_Strategy, Social_Affinity, Resident_Density, Negative_Impact, Ticks, Mortgage_Buyout_Ratio, Social_Quality, Monetary_Valuation, Non_Normalized_Offer, Success, Total_Success, Houses, TwoPlus_Network, Total_Links, Impact_effects, Network_Pref, Distribution, Offer_Adjustment, Hold_out_ratio, Initial_value, Neigh_iso, Link_iso, ThreeH_Network, FourH_Network, Gamma_mean, Gamma_std, Social_Type) VALUES (:Block_Size, :Resident_Strategy, :Social_Affinity, :Resident_Density, :Negative_Impact, :Ticks, :Mortgage_Buyout_Ratio, :Social_Quality, :Monetary_Valuation, :Non_Normalized_Offer, :Success, :Total_Success, :Houses, :TwoPlus_Network, :Total_Links, :Impact_effects, :Network_Pref, :Distribution, :Offer_Adjustment, :Hold_out_ratio, :Initial_value, :Neigh_iso, :Link_iso, :ThreeH_Network, :FourH_Network, :Gamma_mean, :Gamma_std, :Social_Type)"
                cursor.execute(sql, {"Block_Size":Block_Size, "Resident_Strategy":Resident_Strategy, "Social_Affinity":Social_Affinity,"Resident_Density":Resident_Density, "Negative_Impact":Negative_Impact, "Ticks":Ticks, "Mortgage_Buyout_Ratio":Mortgage_Buyout_Ratio, "Social_Quality":Social_Quality, "Monetary_Valuation":Monetary_Valuation, "Non_Normalized_Offer":Non_Normalized_Offer, "Success":Success, "Total_Success":Total_Success, "Houses":Houses, "TwoPlus_Network":TwoPlus_Network, "Total_Links":Total_Links, "Impact_effects":Impact_effects, "Network_Pref":Network_Pref, "Distribution":Distribution, "Offer_Adjustment":Offer_Adjustment, "Hold_out_ratio":Hold_out_ratio, "Initial_value":Initial_value, "Neigh_iso":Neigh_iso, "Link_iso":Link_iso, "ThreeH_Network":ThreeH_Network, "FourH_Network":FourH_Network, "Gamma_mean":Gamma_mean, "Gamma_std":Gamma_std, "Social_Type":Social_Type})    
                conn.commit()
    cursor.close()


def display_all_db_data():
    conn = sqlite3.connect("%smydatabase.db"%file_title)
    cursor = conn.cursor()
    sql = "SELECT * FROM file_title" # * shorthand for all columns of the table
    columns = cursor.execute(sql)
    all_entries = columns.fetchall()
    for entry in all_entries:
        print(entry)  

def display_all_Mortgage_Buyout_Ratio():
    conn = sqlite3.connect("%smydatabase.db"%file_title)
    cursor = conn.cursor()
    sql = "SELECT Mortgage_Buyout_Ratio FROM file_title" # * shorthand for all columns of the table
    Mortgage_Buyout_Ratio_column = cursor.execute(sql)
    all_Mortgage_Buyout_Ratio = Mortgage_Buyout_Ratio_column.fetchall()
    for Mortgage_Buyout_Ratio in all_Mortgage_Buyout_Ratio:
        print(Mortgage_Buyout_Ratio)

def display_min_max_total_number_of_Non_Normalized_Offer():
    #we can use Python to manipulate the data or we can use native SQL functions
    conn = sqlite3.connect("%smydatabase.db"%file_title)
    cursor = conn.cursor()
    sql1 = "SELECT MIN(Non_Normalized_Offer) FROM file_title"
    sql2 = "SELECT MAX(Non_Normalized_Offer) FROM file_title"
    sql3 = "SELECT SUM(Non_Normalized_Offer) FROM file_title"
    sql4 = "SELECT Count(Non_Normalized_Offer) EXCEPT"
    lowest_number = cursor.execute(sql1).fetchone()[0]
    highest_number = cursor.execute(sql2).fetchone()[0]
    total_number = cursor.execute(sql3).fetchone()[0]
    print(lowest_number, highest_number, total_number)


def main():
    
    create_db_and_table()
    add_simulation_data_to_database("output.csv") # this shoudl be a string but back up the fiels first 
    display_all_db_data()
    print("-------")
    display_all_Mortgage_Buyout_Ratio()
    print("-------")
    display_min_max_total_number_of_Non_Normalized_Offer()
main() 
