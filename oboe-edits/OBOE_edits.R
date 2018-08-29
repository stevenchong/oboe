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


### Create a dataframe to store the URI and label information from OBOE Core, Characteristics and Standards

# Set counter for each ontology file's identifier starting number
oboe_core_counter <- 1
oboe_standards_counter <- 1
oboe_characteristics_counter <- 1


#Read in list of OBOE files
ontology_file_list <- list.files(full.names = TRUE, pattern = "oboe-core", ignore.case = TRUE)

for (ontology_file in ontology_file_list) {
	
	#Get ontology file name
	ontology_name <- basename(ontology_file)
	
	#Read in ontology file as XML
	ontology_file <- read_xml(ontology_file)
	
	# Sets the namespace for the Dublin Core element
	xml_attr(ontology_file, "xmlns:dcterms") <- "http://purl.org/dc/terms/"
	

	### Create dataframe for the Data Properties
	data_properties_df <- xml_find_all(ontology_file, "//owl:DatatypeProperty[@rdf:about]") %>%
		xml_attrs() %>% # get URIs
		{ data.frame(matrix( unlist (.), byrow = TRUE), stringsAsFactors = FALSE ) } %>%
		rename ( data_property_URI = 1) # rename first column
	
	# Create labels and new numerical URIs for the data properties, and add them to the dataframe
	for (row in 1:nrow(data_properties_df)){
		
		data_properties_df$new_data_property_URI[row] <- paste0("http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl_", str_pad(row + 100, 8, pad = "0") )
		data_properties_df$data_property_label[row] <- gsub( ".*#", "", data_properties_df[row, 1]  )
	}

	
	### Create dataframe for the object properties
	object_properties <- xml_find_all(ontology_file, "//owl:ObjectProperty[@rdf:about]")
	
	object_properties_df <- object_properties %>%
		xml_attrs() %>%  # get URIs
		{ data.frame(matrix( unlist (.), byrow = TRUE), stringsAsFactors = FALSE ) } %>%
		rename ( object_property_URI = 1) # rename first column
	
	
	# Create labels and new numerical URIs for the object properties, and add them to the dataframe
	for (row in 1:nrow(object_properties_df)){
		
		object_properties_df$new_object_property_URI[row] <- paste0("http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl_", str_pad(row + 50, 8, pad = "0") )
		object_properties_df$object_property_label[row] <- gsub( ".*#", "", object_properties_df[row, 1]  )
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
	
	### Edit the OWL file
	
	#If there is no rdfs:label annotation property use the fragment identifier to create one
	for (class_node in class_nodes){
		
		# Iterate through table
		for (row in 1:nrow (classes_df)){
			
			# Find the table row that matches the URI to get the correct labels and URIs				
			if (xml_attrs(class_node) == classes_df$original_URI[row] ) {
				label	<- classes_df$original_label[row]	
				original_URI <- classes_df$original_URI[row]
				new_label <- classes_df$new_label[row]
				new_URI <- classes_df$new_URI[row]
			} 
		}
		
		# If the URI contains a hashtag and the class doesn't have a label
		if(grepl("#", xml_attrs(class_node) ) && length (xml_find_first(class_node, "./rdfs:label" )) == 0 ){
			
			# Add the rdfs:label node under the class node
			xml_add_child(class_node, read_xml(paste0("<rdfs:label>", label, "</rdfs:label>") )) %>%
			{ xml_attr(. , "xml:lang") <- "en"}  # add attribute for rdfs:label element 
			
		}	
		
		# Add the dcterms:identifier annotation property to existing classes
		xml_add_child(class_node, read_xml(paste0('<dcterms:identifier>"', original_URI ,'"</dcterms:identifier>') ))
		
		
		#Create equivalent classes for the original OBOE classes, containing only the URI and rdfs:label			
		equivalent_class <- xml_add_sibling(class_node, read_xml(paste0('<owl:Class rdf:about="', new_URI, '">
																																		<rdfs:label xml:lang="en">',new_label,'</rdfs:label>
		 																																</owl:Class>')))
		
		
		# Copy the nodesets from the original class to the equivalent class
		nodes_to_copy <- c("owl:equivalentClass", "owl:disjointWith", "rdfs:comment", "rdfs:subClassOf")
		
		for (node in nodes_to_copy){
			copy_xml_nodeset(class_node, node, equivalent_class)
		}
		
		
		# Add an equivalentClass node to existing classes in the ontology
		xml_add_child(class_node, read_xml(paste0('<owl:equivalentClass><owl:Class>
																							<owl:unionOf rdf:parseType="Collection">
																							<rdf:Description rdf:about="', new_URI, '"/>
																							</owl:unionOf>
																							</owl:Class></owl:equivalentClass>') ))
		
		
		
		# Replace all of the original URIs (for class and object property names) present in the classes containing the numerical identifiers with the new URIs
		new_equivalent_class <- as.character(equivalent_class)
		
		for (row in 1:nrow(classes_df)){
				
				if(grepl(paste0("\\b",classes_df$original_URI[row], "\\b"), new_equivalent_class) ){
				
					new_equivalent_class <- gsub(paste0("\\b",classes_df$original_URI[row],"\\b"), classes_df$new_URI[row], new_equivalent_class)
				}
		}	
		
		
		for (row in 1:nrow(object_properties_df)){
			
			if(grepl(paste0("\\b",object_properties_df$object_property_URI[row], "\\b"), new_equivalent_class) ){
				
				new_equivalent_class <- gsub(paste0("\\b",object_properties_df$object_property_URI[row],"\\b"), object_properties_df$new_object_property_URI[row], new_equivalent_class)
			} 
		}	
		
			
		equivalent_class <- xml_replace(equivalent_class, read_xml(new_equivalent_class) )
	
		# Add an equivalentClass node containing the original URI to the classes containing numerical identifiers
		xml_add_child(equivalent_class, read_xml(paste0('<owl:equivalentClass><owl:Class>																							
																										<owl:unionOf rdf:parseType="Collection">
																										<rdf:Description rdf:about="', original_URI, '"/>
																										</owl:unionOf>
																										</owl:Class></owl:equivalentClass>') ))

		
		
		# Add a the dcterms:identifier annotation property to the equivalent classes
		xml_add_child(equivalent_class, read_xml(paste0('<dcterms:identifier>"', new_URI ,'"</dcterms:identifier>') ))
		
		
		# Add a rdfs:comment annotation property to the equivalent classes
		xml_add_child(equivalent_class, read_xml(paste0('<rdfs:comment>It is recommended that this class is used in lieu of ', original_URI, 
																										' for current and future applications. </rdfs:comment>')))

		
		# Add a rdfs:comment annotation property to existing classes
		xml_add_child(class_node, read_xml(paste0('<rdfs:comment>This class is retained for legacy purposes. ', new_URI, 
																							' is an equivalent class that should be used instead.</rdfs:comment>' )))
		
}
	
	### Create equivalent object properties
	# Iterate through the object properties 
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
		xml_add_child(object_property, read_xml(paste0('<dcterms:identifier>"', object_property_URI ,'"</dcterms:identifier>') ))
			

		# Add a rdfs:label node to each object property node
		xml_add_child(object_property, read_xml(paste0("<rdfs:label>", object_property_label, "</rdfs:label>") )) %>%
		{ xml_attr(. , "xml:lang") <- "en"}  # add attribute for rdfs:label element
	
		#Create equivalent object properties for the original object properties, containing only the URI and rdfs:label			
		equivalent_object_property <- xml_add_sibling(object_property, read_xml(paste0('<owl:ObjectProperty rdf:about="', new_object_property_URI, '">
																																		<rdfs:label xml:lang="en">',object_property_label,'</rdfs:label>
																																		</owl:ObjectProperty>' )))
		
		
		# Copy the nodesets from the original object property to its equivalent object property
		nodes_to_copy <- c("rdf:type", "rdfs:comment", "rdfs:domain", "rdfs:range", "owl:inverseOf")
		
		for (node in nodes_to_copy){
			copy_xml_nodeset(object_property, node, equivalent_object_property)
		}
		

		### Replace all of the original URIs present in the object properties and classes containing the original identifiers with the new URIs
		new_equivalent_object_property <- as.character(object_property)
		
		for (row in 1:nrow(object_properties_df)){
			
			if(grepl(paste0("\\b",object_properties_df$object_property_URI[row], "\\b"), new_equivalent_object_property) ){
				
				new_equivalent_object_property <- gsub(paste0("\\b",object_properties_df$object_property_URI[row],"\\b"), object_properties_df$new_object_property_URI[row], new_equivalent_object_property)
			} 
		}	
		
		for (row in 1:nrow(classes_df)){
			
			if(grepl(paste0("\\b",classes_df$original_URI[row], "\\b"), new_equivalent_object_property) ){
				
				new_equivalent_object_property <- gsub(paste0("\\b",classes_df$original_URI[row],"\\b"), classes_df$new_URI[row], new_equivalent_object_property)
			}
		}	
		
		equivalent_object_property <- xml_replace(equivalent_object_property, read_xml(new_equivalent_object_property) )
		
		
		#Add an owl:equivalentProperty node to the equivalent object properties containing numerical identifiers
		xml_add_child(equivalent_object_property, read_xml(paste0('<owl:equivalentProperty rdf:resource="', object_property_URI ,'"/>') ))
		
		# Add a the dcterms:identifier annotation property to the equivalent object properties
		xml_add_child(equivalent_object_property, read_xml(paste0('<dcterms:identifier>"', new_object_property_URI ,'"</dcterms:identifier>') ))
		
		# Add a rdfs:comment annotation property to existing object properties
		xml_add_child(object_property, read_xml(paste0('<rdfs:comment>This object property is retained for legacy purposes. ', new_object_property_URI, 
																							' is an equivalent object property that should be used instead.</rdfs:comment>' )))

		# Add a rdfs:comment annotation property to the equivalent object properties
		xml_add_child(equivalent_object_property, read_xml(paste0('<rdfs:comment>It is recommended that this class is used in lieu of ', object_property_URI, 
																										' for current and future applications. </rdfs:comment>')))
	
		#Add an owl:equivalentProperty node to the original object properties
		xml_add_child(object_property, read_xml(paste0('<owl:equivalentProperty rdf:resource="', new_object_property_URI ,'"/>') ))
		
		}
	
	
	#Create output file name	
	output_filename <- #list.files(full.names = TRUE, pattern = ontology_name, ignore.case = TRUE) %>%
		ontology_name %>%
		{ gsub("\\.owl", "", .) } %>% #removes .owl file extension
		{ gsub(" ", "_" , .) } %>% #replace spaces with "_"
		{ paste0( . ,"_edited.owl" ) }
	
	# Create subdirectory to store the edited OWL files
	subDir <- "/edited_OBOE_files"
	dir.create(file.path(getwd(),subDir), showWarnings = FALSE)
	
	
	# Create output OWL file
	write_xml(ontology_file, file = file.path(paste0(getwd(), subDir), output_filename) )
	
}

