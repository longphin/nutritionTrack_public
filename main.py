execfile("dbCon.py")

fieldsToGet="item_id,item_name,brand_name,n_calories,nf_calories_from_fat,nf_calcium_dv,nf_cholesterol,nf_dietary_fiber,nf_iron_dv,nf_monounsaturated_fat,nf_polyunsaturated_fat,nf_potassium,nf_protein,nf_saturated_fat,nf_serving_size_qty,nf_serving_size_unit,nf_sodium,nf_sugars,nf_total_carbohydrate,nf_total_fat,nf_trans_fatty_acid,nf_vitamin_a_dv,nf_vitamin_c_dv"

def searchNix(queries):
  res=[]
  for query in queries:
    items=nix.search(query, results="0:10", fields=fieldsToGet, filters={"item_type":3}, sort={"field":"_score"}).json()
    if 'hits' in items:
      facts=[item['fields'] for item in items['hits']]
      res.append(facts)
    else:
      res.append({})
#    if(len(res)<10):
      #      items=nix.search(query, results="0:10", fields=fieldsToGet, item_type=1, sort={"field":"_score"}).json()
  return(res)

# main
def main(queries):
  # find items using Nix
  nixItems=searchNix(queries)
  return(nixItems)

