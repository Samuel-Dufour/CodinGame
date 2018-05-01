namespace CodinGame

module PhoneNumbers =
    open System.IO

    type TreeItem =
    | Leaf of int
    | Node of int * TreeItem list

    let countNodes (numbers:string list) =
        let toDigits number =
                number
                |> List.ofSeq
                |> List.map (string >> int)
        let numbers =
            numbers
            |> List.map toDigits
        let addNumberToTree tree number =
            let toTree digits =
                let digits = List.rev digits

                let rec recToTree digits tree =
                    match digits with
                    | [] -> tree
                    | head :: tail -> 
                        match tree with
                        | None -> recToTree tail (Some(Leaf head))
                        | Some item ->
                            recToTree tail (Some((Node (head, [item]))))

                recToTree digits None
            let addNewTreeOrNot tree number = 
                match number |> toTree with
                | None -> tree
                | Some(numberTree) -> numberTree :: tree

            let rec recAddNumberToTree tree digits cont =
                match digits with
                | [] -> cont(tree)
                | _ ->
                let existingNodes, otherNodes = 
                    tree
                    |> List.partition (fun node -> 
                        match node with
                        | Leaf i | Node (i, _ )-> i = digits.[0])

                match existingNodes with
                | [] -> 
                    cont(addNewTreeOrNot tree digits)
                | [Node (i, nodes)] -> 
                    recAddNumberToTree nodes digits.Tail (fun updatedNode -> 
                        cont(Node (i, updatedNode) :: otherNodes))
                | [Leaf(i)] -> 
                    recAddNumberToTree [] digits.Tail (fun updatedNode -> 
                        cont(Node (i, updatedNode) :: otherNodes))            
                | _ :: _ -> failwith "Should not contains many nodes" 

            recAddNumberToTree tree number id


        let countTreeNodes tree = 
            let rec countTreeNodes tree continuation =
                let countNodeAndSubTrees subNodes nextNodes = countTreeNodes subNodes (fun accHead ->
                    let nodeSum = continuation 1
                    countTreeNodes nextNodes (fun accTail -> 
                        accTail + nodeSum + accHead))

                match tree with
                | [] -> continuation 0
                | Leaf(_) :: nextNodes -> countNodeAndSubTrees [] nextNodes
                | Node (_, subNodes) :: nextNodes -> countNodeAndSubTrees subNodes nextNodes
        
            countTreeNodes tree id            

        let rec addToTree tree numbers =
            match numbers with
            | [] -> tree
            | head :: tail -> 
                let tree = addNumberToTree tree head
                addToTree tree tail
        addToTree [] numbers
        |> countTreeNodes