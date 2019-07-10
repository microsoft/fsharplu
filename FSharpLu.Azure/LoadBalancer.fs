// Copyright (c) Microsoft Corporation.

/// F# helpers to the Azure ARM Load balancer API
module Microsoft.FSharpLu.Azure.LoadBalancer

open Microsoft.Azure.Management.Network
open Microsoft.FSharpLu.Async
open Microsoft.FSharpLu.Logging

/// Get set of available frontend ports on the load balancer
let private getAvailablePublicPorts (l:Microsoft.Azure.Management.Network.Models.LoadBalancer) =
    let allPorts =
        seq { 1 .. 65535 }
        |> Set.ofSeq

    let usedPorts =
        l.LoadBalancingRules
        |> Seq.choose (fun r -> r.BackendPort |> Option.ofNullable)
        |> Set.ofSeq

    allPorts - usedPorts

type RegisterPortRedirectionsResult =
    {
        RequestId : string
        AssignedPorts : int list
        LoadBalancerPoolName : string
    }

/// Register a set of port redirection rules specified by triples of the form (localPort, name, protocol)
let registerPortRedirections tags (c:Microsoft.Azure.Management.Network.INetworkManagementClient) groupName loadBalancerName loadBalancerPoolName (rulesRequest:seq<int * string *string>) =
    async {
        let ruleCount = Seq.length rulesRequest

        let! request = c.LoadBalancers.GetWithHttpMessagesAsync(groupName, loadBalancerName).AsAsync
        let l = request.Body

        l.BackendAddressPools.Add <| Models.BackendAddressPool(Name = loadBalancerPoolName)

        let availablePorts = getAvailablePublicPorts l

        // The frontend IP is configured at the initial deployment of the load balancer
        let frontendIp =
            match l.FrontendIPConfigurations |> Seq.toList with
            | [] -> TraceTags.failwith "Frontend IP configuration missing for specified load balancer"
                                (tags @ ["loadBalancerName", loadBalancerName; "groupName", groupName])
            | fip::_ -> fip

        if ruleCount > Set.count availablePorts then
            failwith "The load balancer does not have enough available ports!"

        // Create port redirect rule in load balancer
        let addRule (assignedPublicPort, (localPort, name, protocol)) =

            let rule = new Models.InboundNatRule(
                                protocol = protocol,
                                backendPort = System.Nullable localPort,
                                frontendIPConfiguration = frontendIp,
                                enableFloatingIP = System.Nullable false,
                                frontendPort = System.Nullable assignedPublicPort,
                                name = name)

            l.InboundNatRules.Add(rule)

        // Assign ports to each rule. This gives a list of the elements: ((localPort, name, protocol), assignedPublicPort)
        let rulesWithAssignedPorts =
            rulesRequest
            |> Seq.zip availablePorts
            |> Seq.toList

        Seq.iter addRule rulesWithAssignedPorts

        let! r = c.LoadBalancers.CreateOrUpdateWithHttpMessagesAsync(groupName, loadBalancerName, l).AsAsync

        return
            {
                RequestId = r.RequestId
                AssignedPorts = List.map fst rulesWithAssignedPorts
                LoadBalancerPoolName = loadBalancerPoolName
            }
    }