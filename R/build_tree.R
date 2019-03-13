#' Convert the objects loaded from YAML fragments into a tree
#'
#' If the [data.tree::data.tree] package is installed, this function
#' can be used to convert a list of objects, as loaded from extracted
#' YAML fragments, into a [data.tree::Node()].
#'
#' @param x Either a list of YAML fragments loaded from a file with
#' [load_yaml_fragments()], or a list of such lists loaded from all files
#' in a directory with [load_yaml_dir()].
#' @param idName The name of the field containing each elements' identifier,
#' used to build the data tree when there are references to a parent from a child
#' element.
#' @param parentIdName The name of the field containing references to an element's
#' parent element (i.e. the field containing the identifier of the corresponding
#' parent element).
#' @param childrenName The name of the field containing an element's children, either
#' as a list of elements, or using the 'shorthand' notation, in which case a vector
#' is supplied with the identifiers of the children.
#' @param autofill A named vector where the names represent fields to fill with
#' the values of the fields specified in the vector values. Note that autofill
#' replacements are only applied if the fields to be autofilled (i.e. the names of
#' the vector specified in `autofill`) do not already have a value.
#' @param rankdir How to plot the plot when it's plotted: the default `"LR"` plots from
#' left to right. Specify e.g. `"TB"` to plot from top to bottom.
#' @param directed Whether the edges should have arrows (`"forward"` or `"backward"`)
#' or not (`"false"`).
#' @param silent Whether to provide (`FALSE`) or suppress (`TRUE`) more detailed progress updates.
#'
#' @return a [data.tree::Node()] object.
#'
#' @examples
#' loadedYum <- yum::load_yaml_fragments(text=c(
#' "---",
#' "-",
#' "  id: firstFragment",
#' "---",
#' "Outside of YAML",
#' "---",
#' "-",
#' "  id: secondFragment",
#' "  parentId: firstFragment",
#' "---",
#' "Also outside of YAML"));
#' yum::build_tree(loadedYum);
#' @export
build_tree <- function(x,
                       idName = 'id',
                       parentIdName = 'parentId',
                       childrenName = 'children',
                       autofill = c(label = 'id'),
                       rankdir="LR",
                       directed="false",
                       silent=TRUE) {

  if (!requireNamespace("data.tree", quietly = TRUE)) {
    stop("To build a tree, the \"data.tree\" package is required. ",
         "Please install it using `install.packages('data.tree');`.",
         call. = FALSE);
  }

  if ("simplifiedYum" %in% class(x)) {
    ### Nothing more to do - can be processed directly.
  } else if ("yumFromDir" %in% class(x)) {
    x <-
      unlist(x,
             recursive=FALSE);
  } else if (!("yumFromFile" %in% class(x)) &&
             !("yumRecursion" %in% class(x)) &&
             !("yumFromList" %in% class(x))) {
    stop("I can only process the objects resulting from ",
         "a call to 'load_yaml_fragments', 'load_yaml_dir, ",
         " or 'load_yaml_list', which have class ",
         "'yumFromFile', 'yumFromDir', and 'yumFromList'. The ",
         "object you provided has ",
         ifelse(length(class(x)) == 1,
                       "class ",
                       "classes "),
         vecTxtQ(class(x)),
         ".");
  }

  if (is.null(x)) {
    return(x);
  }

  if (!is.null(x[[idName]])) {
    ### We have an identifier, so this is a node in itself. First check
    ### and if necessary, clean up this node.
    if (!silent) {
      cat("Passed object has identifier '",
          x[[idName]],
          "'.",
          sep="");
    }
    for (currentAutofill in names(autofill)) {
      if (is.null(x[[currentAutofill]])) {
        x[[currentAutofill]] <- x[[autofill[currentAutofill]]];
      }
    }
    ### Then, check whether it has children.
    if (is.null(x[[childrenName]])) {
      ### If not, convert this node into a Node and return it.
      res <- data.tree::Node$new(x[[idName]]);
      for (currentSub in (setdiff(names(x),
                                  idName))) {
        res[[currentSub]] <-
          x[[currentSub]];
      }
      return(res);
    } else {
      ### Check whether the children are 'shorthand children', and if
      ### they are, construct the proper sub-object first.
      if (is.atomic(x[[childrenName]])) {
        x[[childrenName]] <-
          lapply(x[[childrenName]],
                 function(childId) {
                   currentChild <- list();
                   currentChild[[idName]] <-
                     childId;
                   for (currentAutofill in names(autofill)) {
                     currentChild[[currentAutofill]] <-
                       childId;
                   }
                   return(currentChild);
                 });
      }
      if (!silent) {
        cat("Converting it to a data.tree Node and returning it.");
      }
      ### Then convert this node into a Node
      res <-
        data.tree::FromListExplicit(x,
                                    nameName=idName,
                                    childrenName=childrenName);

      if (!silent) {
        cat("Tree root object has name '",
            res$name,
            "'.", sep="");
      }

      ### Check for missing labels and/or codes and fill them with the identifiers
      res$Do(function(node) {

        for (currentAutofill in names(autofill)) {
          if (is.null(node[[currentAutofill]])) {
            if (autofill[currentAutofill] == idName) {
              node[[currentAutofill]] <-
                node$name;
            } else {
              node[[currentAutofill]] <-
                node[[autofill[currentAutofill]]];
            }
          }
        }
      });
      ### Set plotting styles
      data.tree::SetGraphStyle(res,
                               directed = directed);
      data.tree::SetGraphStyle(res,
                               rankdir = rankdir);

      ### Return the result
      return(res);
    }
  } else {
    ### This is a list of nodes, so pass each on to this function and
    ### collect the results; then start building the tree.
    if (!silent) {
      cat("Passed object does not have an identifier; processing it as a list of objects.");
    }

    nodeList <-
      lapply(lapply(x,
                    function(xToStructure) {
                      if (is.null(xToStructure)) {
                        return(structure(list(),
                                         class = 'yumRecursion'));
                      } else {
                        return(structure(xToStructure,
                                         class = 'yumRecursion'));
                      }
                    }),
             build_tree,
             idName = 'id',
             parentIdName = parentIdName,
             childrenName = childrenName,
             autofill = autofill,
             silent = silent);

    nodeIds <-
      data.tree::Get(nodeList,
                     'name');

    if (!silent) {
      cat("Processed ",
          length(nodeList),
          " nodes (",
          vecTxtQ(nodeIds),
          ").", sep="");
    }

    ### If it's a single node, just return it immediately.
    if (length(nodeList) == 1) {
      if (!silent) {
        cat("Single node, so returning it.");
      }
      return(nodeList[[1]]);
    }

    ### Create the data tree object
    resTree <- data.tree::Node$new();

    ### Add all children of nodes without an id as children of the resTree root
    if (any(nchar(nodeIds)==0)) {
      for (nodeWithoutId in nodeList[nchar(nodeIds)==0]) {
        if (!silent) {
          cat("Found a set of nodes without identifiers; adding the children to the root of the code tree.");
        }
        for (subNodeWithoutId in nodeWithoutId$children) {
          resTree$AddChildNode(subNodeWithoutId);
          if (!silent) {
            cat("Added '",
                subNodeWithoutId$name,
                "' to the root of the code tree.\n", sep="");
          }
        }
      }
      ### Then remove them from the nodeList
      nodeList <-
        nodeList[nchar(nodeIds)>0];
    }

    ### Check which nodes have a parent
    parentIds <-
      data.tree::Get(nodeList,
                     parentIdName);

    nodesWithoutParents <-
      nodeList[unlist(is.na(parentIds))];
    nodesWithParents <-
      nodeList[unlist(!is.na(parentIds))];

    ### Attach those that don't to the root.
    for (i in nodesWithoutParents) {
      resTree$AddChildNode(i);
      if (!silent) {
        cat("Attached parentless node '",
            i$name,
            "' to the root of the code tree.");
      }
    }
    ### For those that do, insert them at the appropriate place.
    for (i in seq_along(nodesWithParents)) {
      if (!silent) {
        cat("Starting to process node '",
            nodesWithParents[[i]]$name,
            "' to find its parent '",
            nodesWithParents[[i]][[parentIdName]],
            "'.\n",
            sep="");
      }
      parentNode <-
        data.tree::FindNode(resTree,
                            nodesWithParents[[i]][[parentIdName]]);
      if (!is.null(parentNode)) {
        ### Parent is already in the coding tree; attach this child node.
        parentNode$AddChildNode(nodesWithParents[[i]]);
        if (!silent) {
          cat("Attached node '",
              nodesWithParents[[i]]$name,
              "' to its parent node '",
              parentNode$name,
              "' in the code tree.\n",
              sep="");
        }
      } else {
        ### Parent is not in the coding tree; look in the other nodes with
        ### parents that we still have to process
        if (i == length(nodesWithParents)) {
          stop(paste0("Node with identifier '", nodesWithParents[[i]]$name,
                      "' has specified parent '", nodesWithParents[[i]][[parentIdName]],
                      "' but no node with that identifier exists."));
        } else {
          foundParent <- FALSE;
          for (j in (i+1):length(nodesWithParents)) {
            parentNode <-
              data.tree::FindNode(nodesWithParents[[j]],
                                  nodesWithParents[[i]][[parentIdName]]);
            if (!is.null(parentNode)) {
              ### Parent is not yet in the coding tree; attach this child node.
              parentNode$AddChildNode(nodesWithParents[[i]]);
              foundParent <- TRUE;
              if (!silent) {
                cat("Attached node '",
                    nodesWithParents[[i]]$name,
                    "' to its parent node '",
                    parentNode$name,
                    "', for now outside the code tree.",
                    sep="");
              }
            }
          }
          if (!foundParent) {
            print(parentNode);
            stop(paste0("Node with identifier '", nodesWithParents[[i]]$name,
                        "' has specified parent '", nodesWithParents[[i]][[parentIdName]],
                        "' but no node with that identifier exists (all node identifiers are ",
                        vecTxtQ(nodeIds), ")."));
          }
        }
      }
    }

    ### Set plotting styles
    data.tree::SetGraphStyle(resTree,
                             directed = directed);
    data.tree::SetGraphStyle(resTree,
                             rankdir = rankdir);

    ### Return the result
    return(resTree);
  }
}
