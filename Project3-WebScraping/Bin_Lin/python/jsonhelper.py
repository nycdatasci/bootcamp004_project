import json

def simple_dict_to_object(dic, obj):    
    
    obj_dict = obj.__dict__
    
    for key, value in obj_dict.iteritems():
        if(not dic.get(key) == None):
            obj_dict[key] = dic[key]
        
    return obj

def simple_json2obj(jsonData, obj):
    dic = json.dumps(jsonData)
    return json.loads(dic, object_hook=(lambda d: simple_dict_to_object(d, obj)))


