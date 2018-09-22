### This script extracts class URIs and creates a Dublin Core identifier annotation field. For classes that do not have an rdfs:label
### field but have fragment identifiers in their URIs, the script will create an rdfs:label based on the fragment. It also replaces references
### to the original URIs in other element attributes containing the new URIs.  The script takes as input an OWL file in RDF/XML syntax. It outputs 
### an OWL file.

### NOTE: The output OWL file will need to have its internal document type definition updated manually. The following line should
### be copied and pasted to the output file:
### <!ENTITY dcterms "http://purl.org/dc/terms/"

### To fix the output style formatting when viewing the OWL file in a text editor, the file can be viewed and exported in Protege.
### Be aware that this may erase the internal document type definition!  


#Libraries
library(dplyr)
library(xml2)
library(XML)
library(stringr)

### Functions
# Function to copy nodesets. The source is where the nodeset is located. The nodeset_to_copy is the name of the top-level element in the nodeset.
# The destination_node is where to insert the nodeset, as a child node.
copy_xml_nodeset <- function(source, nodeset_to_copy, destination_node ) {
	
	temp_nodeset <- xml_find_all(source, paste0("./", nodeset_to_copy))

	for (child in temp_nodeset){
		xml_add_child(destination_node, child)
	}
	
}	


#Function for replacing the original URIs in classes. The source is where the nodeset is located. The nodeset_to_check is the name of the top-level element
#in the nodeset containing the class URIs to replace. The destination_node is the location of where to insert the nodeset, as a child node.
replace_class_URIs <- function(source, nodeset_to_check, destination_node ) {

	temp_nodeset <- xml_find_all(source, paste0("./", nodeset_to_check))
	
	for (child in temp_nodeset){
		
		child <- gsub("&oboe-core;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#", child) %>%
		{ gsub("&oboe-characteristics;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-characteristics.owl#", . ) } %>%
		{ gsub("&oboe-standards;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-standards.owl#", . ) }
		
		for (counter in 1:nrow(combined_classes_df)){
			if(grepl(paste0("\\b",combined_classes_df$original_URI[counter],"\\b"),child )){
				
				child	<- gsub(combined_classes_df$original_URI[counter], combined_classes_df$new_URI[counter], child)
				
			}
		}
		
		xml_add_child(destination_node, child)
	}
	
}


#Function for replacing the original URIs in object properties. The source is where the nodeset is located. The nodeset_to_check is the name of the top-level element
#in the nodeset containing the object property URIs to replace. The destination_node is the location of where to insert the nodeset, as a child node.
replace_object_property_URIs <- function(source, nodeset_to_check, destination_node ) {
	
	temp_nodeset <- xml_find_all(source, paste0("./", nodeset_to_check))
	
	for (child in temp_nodeset){
		
		child <- gsub("&oboe-core;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#", child) %>%
		{ gsub("&oboe-characteristics;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-characteristics.owl#", . ) } %>%
		{ gsub("&oboe-standards;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-standards.owl#", . ) }
		
		
		for (counter in 1:nrow(object_properties_df)){
			if(grepl(paste0("\\b",object_properties_df$object_property_URI[counter],"\\b"),child )){
				
				child	<- gsub(object_properties_df$object_property_URI[counter], object_properties_df$new_object_property_URI[counter], child)
				
			}
		}
		
		xml_add_child(destination_node, child)
	}
	
}


#Function for replacing the original URIs in data properties. The source is where the nodeset is located. The nodeset_to_check is the name of the top-level element
#in the nodeset containing the object property URIs to replace. The destination_node is the location of where to insert the nodeset, as a child node.
replace_data_property_URIs <- function(source, nodeset_to_check, destination_node ) {
	
	temp_nodeset <- xml_find_all(source, paste0("./", nodeset_to_check))
	
	for (child in temp_nodeset){
		
		child <- gsub("&oboe-core;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#", child) %>%
		{ gsub("&oboe-characteristics;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-characteristics.owl#", . ) } %>%
		{ gsub("&oboe-standards;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-standards.owl#", . ) }
		
		
		for (counter in 1:nrow(data_properties_df)){
			if(grepl(paste0("\\b", data_properties_df$data_property_URI[counter],"\\b"),child )){
				
				child	<- gsub(data_properties_df$data_property_URI[counter], data_properties_df$new_data_property_URI[counter], child)
				
			}
		}
		
		xml_add_child(destination_node, child)
	}
	
}


#Function for adding a dcterms:identifier node. The node is the location where the identifier will be added to. The URI is the identifier to display.
add_identifier_node <- function (node, URI){
	xml_add_child(node, read_xml(paste0('<dcterms:identifier>"', URI ,'"</dcterms:identifier>') ))
}

#Function for adding a rdfs:label node. The node is the location where the label will be added to. The label is the label text.
add_label_node <- function (node, label) {

	xml_add_child(node, read_xml(paste0("<rdfs:label>", label, "</rdfs:label>") )) %>%
		{ xml_attr(. , "xml:lang") <- "en"}  # add attribute for rdfs:label element 
}

#Function for adding a comment to legacy classes and properties. The node is the location where the comment will be added to. The ontology_section
# is a character string that describes the part of the ontology that is involved (e.g. "class" or "object property"). The URI is the replacement URI 
# that should be used instead.
add_legacy_comment_node <- function (node, ontology_section, URI) {

	xml_add_child(node, read_xml(paste0('<rdfs:comment>This ', ontology_section, ' is retained for legacy purposes. ', URI, 
																						 ' is an equivalent ', ontology_section, ' that should be used instead.</rdfs:comment>' )))
}

#Function for adding a comment to replacement classes and properties. The node is the location where the comment will be added to. The ontology_section
# is a character string that describes the part of the ontology that is involved (e.g. "class" or "object property"). The URI is the replacement URI 
# that should be used instead.
add_replacement_comment_node <- function (node, ontology_section, URI) {

	xml_add_child(node, read_xml(paste0('<rdfs:comment>It is recommended that this ', ontology_section, ' is used in lieu of ', URI, 
																												' for current and future applications. </rdfs:comment>')))
}

#Function for adding an equivalent class node. The node is where the equivalent class node will be added to. The URI is the identifier of the equivalent class.
add_equivalent_class_node <- function (node, equivalent_class_URI) {

	xml_add_child(node, read_xml(paste0('<owl:equivalentClass><owl:Class>																							
																								<owl:unionOf rdf:parseType="Collection">
																								<rdf:Description rdf:about="', equivalent_class_URI, '"/>
																								</owl:unionOf>
																								</owl:Class></owl:equivalentClass>') ))
}

#Function for adding an equivalentProperty node. The node is where the equivalent property node will be added to. The URI is the identifier of the equivalent property.
add_equivalent_property_node <- function (node, equivalent_property_URI){
	
	xml_add_child(node, read_xml(paste0('<owl:equivalentProperty rdf:resource="', equivalent_property_URI ,'"/>') ))
}
	

# Set counter for each ontology file's identifier starting number
oboe_core_counter <- 1
oboe_standards_counter <- 1
oboe_characteristics_counter <- 1

combined_classes_list <- list()


#Read in list of OBOE files
ontology_file_list <- list.files(full.names = TRUE, pattern = "_oboe-", ignore.case = TRUE)

for (i in 1:length(ontology_file_list)) {
	
	#Get ontology file name
	ontology_name <- basename(ontology_file_list[i])
	
	
	#Read in ontology file as XML
	ontology_file <- read_xml(ontology_file_list[i])

	
	### Create dataframes to store the URI and label information from OBOE Core, Characteristics and Standards classes and properties
		
	### Create dataframe for the Data Properties
	data_properties <- xml_find_all(ontology_file, "//owl:DatatypeProperty[@rdf:about]")
	
	# If there are data properties present, create a dataframe
	if (length(data_properties) != 0) {
		
		data_properties_df <- data_properties %>%
			xml_attrs() %>% # get URIs
			{ data.frame(matrix( unlist (.), byrow = TRUE), stringsAsFactors = FALSE ) } %>%
			rename ( data_property_URI = 1) # rename first column
		
		# Create labels and new numerical URIs for the data properties, and add them to the dataframe
		for (row in 1:nrow(data_properties_df)){
			
			data_properties_df$new_data_property_URI[row] <- paste0("http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl_", str_pad(row + 100, 8, pad = "0") )
			data_properties_df$data_property_label[row] <- gsub( ".*#", "", data_properties_df[row, 1]  )
		}
	
	} 

	
	### Create dataframe for the object properties
	object_properties <- xml_find_all(ontology_file, "//owl:ObjectProperty[@rdf:about]")
	
	# If there is are any object properties, create a dataframe
	if (length(object_properties) != 0) {
	
		object_properties_df <- object_properties %>%
			xml_attrs() %>%  # get URIs
			{ data.frame(matrix( unlist (.), byrow = TRUE), stringsAsFactors = FALSE ) } %>%
			rename ( object_property_URI = 1) # rename first column
		
		
		# Create labels and new numerical URIs for the object properties, and add them to the dataframe
		for (row in 1:nrow(object_properties_df)){
			
			object_properties_df$new_object_property_URI[row] <- paste0("http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl_", str_pad(row + 50, 8, pad = "0") )
			object_properties_df$object_property_label[row] <- gsub( ".*#", "", object_properties_df[row, 1]  )
		}
	
	}
		
	
	### Create dataframe for the classes that maps old URIs and labels to their transformed equivalents
	class_nodes <- xml_find_all(ontology_file, "//owl:Class[@rdf:about]")
	
	classes_df <- class_nodes %>%
		xml_attrs() %>%
		{ data.frame(matrix( unlist (.), byrow = TRUE), stringsAsFactors = FALSE ) } %>%
		rename ( original_URI = 1)
	
	for (row in 1:nrow(classes_df)){
		
		#If the class has a label
		if(	length (xml_find_all(class_nodes[row], "./rdfs:label" )) != 0 ) {
			
			# Extract the label text
			label <- as.character(xml_find_first(class_nodes[row], "./rdfs:label/text()") )
			
			# If there is no label
		} else {
			
			# Extract the text after the fragment identifier for each class
			label <- gsub( ".*#", "", as.character(xml_attrs(class_nodes[row]) ) )
			
		}
		
		# Clean the label text
		label <- gsub('([[:upper:]])', ' \\1', label) %>%
			trimws()
		
		# Create a new, formatted label
		new_label <- tolower(label)%>%
		  { gsub('  ', ' ', .) } %>%
			trimws()
		
		# Add the labels to the table
		classes_df$original_label[row] <- label
		classes_df$new_label[row] <- new_label
	
		
		### Create a numerical URI for each class
		# If editing OBOE-Core
		if (grepl("oboe-core", classes_df$original_URI[row], ignore.case = TRUE)) {
			
			new_URI <-	paste0("http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl_",str_pad(oboe_core_counter, 8, pad = "0") ) 
			oboe_core_counter <- oboe_core_counter + 1
		}
		
		# If editing OBOE-Standards
		if (grepl("oboe-standards", classes_df$original_URI[row], ignore.case = TRUE)) {
			
			new_URI <-	paste0("http://ecoinformatics.org/oboe/oboe.1.2/oboe-standards.owl_",str_pad(oboe_standards_counter, 8, pad = "0") ) 
			oboe_standards_counter <- oboe_standards_counter + 1
		}
		
		# If editing OBOE-Characteristics
		if (grepl("oboe-characteristics", classes_df$original_URI[row], ignore.case = TRUE)) {
			
			new_URI <-	paste0("http://ecoinformatics.org/oboe/oboe.1.2/oboe-characteristics.owl_",str_pad(oboe_characteristics_counter, 8, pad = "0") ) 
			oboe_characteristics_counter <- oboe_characteristics_counter + 1
		}
		
		# Add the new URI to the table
		classes_df$new_URI[row] <- new_URI
		
	}
	
	# Reorder the columns in the table
	classes_df <- select(classes_df, original_URI, new_URI, original_label, new_label)
	
	# Add each class dataframe to the list of dataframes
	combined_classes_list[[i]] <- classes_df

}

# Combine the class dataframes
combined_classes_df <- do.call(bind_rows, combined_classes_list)
	

### Edit each OWL file
for (i in 1:length(ontology_file_list)) {	

	#Get ontology file name
	ontology_name <- basename(ontology_file_list[i])
	
	#Read in ontology file as XML
	ontology_file <- read_xml(ontology_file_list[i])
	
	#Find all the classes in the OWL file
	class_nodes <- xml_find_all(ontology_file, "//owl:Class[@rdf:about]")
	
	# Sets the namespace for the Dublin Core element
	xml_attr(ontology_file, "xmlns:dcterms") <- "http://purl.org/dc/terms/"
	
	
	#Iterate through the classes
	for (class_node in class_nodes){
		
		# Iterate through table
		for (row in 1:nrow (combined_classes_df)){
			
			# Find the table row that matches the URI to get the correct labels and URIs				
			if (xml_attrs(class_node) == combined_classes_df$original_URI[row] ) {
				label	<- combined_classes_df$original_label[row]	
				original_URI <- combined_classes_df$original_URI[row]
				new_label <- combined_classes_df$new_label[row]
				new_URI <- combined_classes_df$new_URI[row]
			} 
		}
		
		
		# If the URI contains a hashtag and the class doesn't have a label
		if(grepl("#", xml_attrs(class_node) ) && length (xml_find_first(class_node, "./rdfs:label" )) == 0 ){
			
			# Add the rdfs:label node under the class node
			add_label_node(class_node, label)
			
		}	
		
		
		# Add the dcterms:identifier annotation property to existing classes
		add_identifier_node(class_node, original_URI)
	

		
		#Create equivalent classes for the original OBOE classes, containing only the URI and rdfs:label			
		equivalent_class <- xml_add_sibling(class_node, read_xml(paste0('<owl:Class rdf:about="', new_URI, '">
																																		<rdfs:label xml:lang="en">',new_label,'</rdfs:label>
		 																																</owl:Class>')))

		
		# Copy the nodesets from the original class to the equivalent class
		nodes_to_copy <- c("owl:equivalentClass",  "rdfs:comment")
		
		for (node in nodes_to_copy){
			
			copy_xml_nodeset(class_node, node, equivalent_class)
		}
		
		# Replace original URIs in the owl:disjointWith nodes 
		nodes_to_replace_URIs <- c("owl:disjointWith")
		
		for (node in nodes_to_replace_URIs){
			
			replace_class_URIs(class_node, node, equivalent_class)
		}

		
		# Replace URIs in the subclass nodes, which have both object properties and classes
		subclasses <- xml_find_all(class_node, "./rdfs:subClassOf") %>%
		{	gsub("&oboe-core;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#", .) }%>%
		{ gsub("&oboe-characteristics;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-characteristics.owl#", . ) } %>%
		{ gsub("&oboe-standards;", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-standards.owl#", . ) }
		
		# Iterate through each subclass node in each class
		for (subclass in subclasses) {
			
			for (counter in 1:nrow(combined_classes_df)){
				if(grepl(paste0("\\b",combined_classes_df$original_URI[counter],"\\b"), subclass )){

					subclass <- gsub(combined_classes_df$original_URI[counter],combined_classes_df$new_URI[counter], subclass)
				} 
			}

			for (counter in 1:nrow(object_properties_df)){
				
				if(grepl(paste0("\\b",object_properties_df$object_property_URI[counter],"\\b"),subclass)){
					
					subclass <- gsub(object_properties_df$object_property_URI[counter], object_properties_df$new_object_property_URI[counter], subclass)
				} 
			}
			
			for (counter in 1:nrow(data_properties_df)){
				
				if(grepl(paste0("\\b",data_properties_df$data_property_URI[counter],"\\b"),subclass)){
					
					subclass <- gsub(data_properties_df$data_property_URI[counter], data_properties_df$new_data_property_URI[counter], subclass)
				} 
			}
			
			xml_add_child(equivalent_class , subclass)
		}
		
	
		# Add an equivalentClass node to existing classes in the ontology
		add_equivalent_class_node(class_node, new_URI)
		
			
		# Add an equivalentClass node containing the original URI to the classes containing numerical identifiers
		add_equivalent_class_node(equivalent_class, original_URI)
		
		
		# Add a the dcterms:identifier annotation property to the equivalent classes
		add_identifier_node(equivalent_class, new_URI)
		
		# Add a rdfs:comment annotation property to the equivalent classes
		add_replacement_comment_node(equivalent_class, "class", original_URI)
		
		# Add a rdfs:comment annotation property to existing classes
		add_legacy_comment_node(class_node, "class", new_URI )
	
		}

	
	### Create equivalent object properties
	# Find all of the object properties in the OWL file
	object_properties <- xml_find_all(ontology_file, "//owl:ObjectProperty[@rdf:about]")
	
	# Iterate through the object properties in each OWL file 
	for (object_property in object_properties){
	
		for (row in 1:nrow(object_properties_df)){
			
			# Find the table row that matches the URI to get the correct labels and URIs
			if (xml_attrs(object_property) == object_properties_df$object_property_URI[row] ) {
				object_property_label <- object_properties_df$object_property_label[row]
				object_property_URI <- object_properties_df$object_property_URI[row]
				new_object_property_URI <- object_properties_df$new_object_property_URI[row]
			}
		}
			
			
		# Add in the dcterms:identifier to the original object properties
		add_identifier_node(object_property, object_property_URI)

		# Add a rdfs:label node to each object property node
		add_label_node(object_property, object_property_label)
	
		#Create equivalent object properties for the original object properties, containing only the URI and rdfs:label			
		equivalent_object_property <- xml_add_sibling(object_property, read_xml(paste0('<owl:ObjectProperty rdf:about="', new_object_property_URI, '">
																																		<rdfs:label xml:lang="en">',object_property_label,'</rdfs:label>
																																		</owl:ObjectProperty>' )))
		
		
		# Copy the nodesets from the original object property to its equivalent object property
		nodes_to_copy <- c("rdf:type", "rdfs:comment" )
		
		for (node in nodes_to_copy){
			copy_xml_nodeset(object_property, node, equivalent_object_property)
		}
		
		# Replace the object property URIs in the owl:inverseOf fields in the object properties
		replace_object_property_URIs(object_property, "owl:inverseOf", equivalent_object_property)
		
		# Replace the class URIs in the rdfs:domain and rdfs:range fields in the object properties
		nodes_to_replace_URIs <- c("rdfs:domain", "rdfs:range")
		
		for (node in nodes_to_replace_URIs){
			replace_class_URIs(object_property, node, equivalent_object_property)
		}		
		

		#Add an owl:equivalentProperty node to the equivalent object properties containing numerical identifiers
		add_equivalent_property_node(equivalent_object_property, object_property_URI)
		
		# Add a the dcterms:identifier annotation property to the equivalent object properties
		add_identifier_node(equivalent_object_property, new_object_property_URI)
		
		# Add a rdfs:comment annotation property to existing object properties
		add_legacy_comment_node(object_property, "object property", new_object_property_URI)

		# Add a rdfs:comment annotation property to the equivalent object properties
		add_replacement_comment_node(equivalent_object_property, "object property", object_property_URI)
	
		#Add an owl:equivalentProperty node to the original object properties
		add_equivalent_property_node(object_property, new_object_property_URI)
		
		}
	
	
	### Create equivalent data properties
	# Find all of the data properties in the OWL file
	data_properties <- xml_find_all(ontology_file, "//owl:DatatypeProperty[@rdf:about]")
	
	# Iterate through the data properties in each OWL file 
	for (data_property in data_properties){
		for (row in 1:nrow(data_properties_df)){
			
			# Find the table row that matches the URI to get the correct labels and URIs
			if (xml_attrs(data_property) == data_properties_df$data_property_URI[row] ) {
				data_property_label <- data_properties_df$data_property_label[row]
				data_property_URI <- data_properties_df$data_property_URI[row]
				new_data_property_URI <- data_properties_df$new_data_property_URI[row]
			}
		}
	
		# Add in the dcterms:identifier to the original data properties
		add_identifier_node(data_property, data_property_URI)	
		
		# Add a rdfs:label node to each data property node
		add_label_node(data_property, data_property_label)
		
		#Create equivalent data properties for the original data properties, containing only the URI and rdfs:label			
		equivalent_data_property <- xml_add_sibling(data_property, read_xml(paste0('<owl:DatatypeProperty rdf:about="', new_data_property_URI, '">
																																									 <rdfs:label xml:lang="en">',data_property_label,'</rdfs:label>
																																									 </owl:DatatypeProperty>' )))
		
		# Copy the nodesets from the original data property to its equivalent data property
		nodes_to_copy <- c("rdf:type", "rdfs:comment", "rdfs:range")
		
		for (node in nodes_to_copy){
			copy_xml_nodeset(data_property, node, equivalent_data_property)
		}
		
		### Replace the original class URIs present in the data properties and classes containing the original identifiers with the new URIs
		replace_class_URIs(data_property, "rdfs:domain", equivalent_data_property )
		
		#Add an owl:equivalentProperty node to the equivalent data properties containing numerical identifiers
		add_equivalent_property_node(equivalent_data_property, data_property_URI)
		
		# Add a the dcterms:identifier annotation property to the equivalent data properties
		add_identifier_node(equivalent_data_property, new_data_property_URI)
		
		# Add a rdfs:comment annotation property to existing data properties
		add_legacy_comment_node(data_property, "data property", new_data_property_URI)
		
		# Add a rdfs:comment annotation property to the equivalent data properties
		add_replacement_comment_node(equivalent_data_property, "data property", data_property_URI)
		
		#Add an owl:equivalentProperty node to the original data properties
		add_equivalent_property_node(data_property, new_data_property_URI)
		
	}
	
	# Remove extra tags
	ontology_file <- gsub("<<", "<", ontology_file) %>%
		{ gsub(">/>", ">", .) } %>%
		read_xml()
	
	
	#Create output file name	
	output_filename <- ontology_name %>%
		{ gsub(".*_", "", . ) } %>%
		{ gsub("\\.owl", "", .) } %>% #removes .owl file extension
		{ gsub(" ", "_" , .) } %>% #replace spaces with "_"
		{ paste0( . ,"_edited.owl" ) }
	
	# Create subdirectory to store the edited OWL files
	subDir <- "/edited_OBOE_files"
	dir.create(file.path(getwd(),subDir), showWarnings = FALSE)
	
	
	# Create output OWL file
	write_xml(ontology_file, file = file.path(paste0(getwd(), subDir), output_filename) )

}	

