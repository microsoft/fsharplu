/// F# helpers to the Azure Network API
/// Copyright (c) Microsoft Corporation.
module Microsoft.FSharpLu.Azure.Network

open System

open Microsoft.FSharpLu
open Microsoft.FSharpLu.ErrorHandling
open Microsoft.Azure.Management.Network.Models
open Microsoft.FSharpLu.Azure
open Microsoft.FSharpLu.Azure.AppInsights
open Microsoft.FSharpLu.Azure.Context
open Microsoft.FSharpLu.Azure.Request
open Microsoft.FSharpLu.HttpCommunication
open System.Net

/// Container for network security rule properties
type NetworkSecurityRule = {

    Protocol: string

    SourceAddressPrefix: string

    SourcePortRange: string

    DestinationAddressPrefix: string

    DestinationPortRange: string

    Priority: Nullable<int>

    Access: string

    Direction: string

    Name: string
}

/// This retry policy handles both 429 (too many requests) and Conflict errors, both of
/// these errors we have seen when trying to update the NSG.
let networkSecurityGroupRetryPolicy =
    Microsoft.Rest.TransientFaultHandling.RetryPolicy(
        HttpTransientErrorDetectionStrategy [enum Constants.TooManyRequestHttpStatus; HttpStatusCode.Conflict],
        Microsoft.Rest.TransientFaultHandling.ExponentialBackoffRetryStrategy())

/// Add a security rule to the given NSG
let addSecurityRule (context:InfrastructureContext) groupName networkSecurityGroup ruleName (properties:NetworkSecurityRule) =
    async {
        let securityRule = SecurityRule(Protocol = properties.Protocol,
                                        SourceAddressPrefix = properties.SourceAddressPrefix,
                                        SourcePortRange = properties.SourcePortRange,
                                        DestinationAddressPrefix = properties.DestinationAddressPrefix,
                                        DestinationPortRange = properties.DestinationPortRange,
                                        Priority = properties.Priority,
                                        Access = properties.Access,
                                        Direction = properties.Direction,
                                        Name = properties.Name)

        return! context.network.SecurityRules.CreateOrUpdateWithHttpMessagesAsync(groupName,
                                                                                  networkSecurityGroup,
                                                                                  ruleName,
                                                                                  securityRule) |> Async.AwaitTask
    }

/// Get security rules starting with the specified prefix from a given NSG
let getSecurityRule (context:InfrastructureContext) groupName networkSecurityGroupName (rulePrefix:string) =
    async {
        let! nsg = context.network.NetworkSecurityGroups.GetWithHttpMessagesAsync(groupName, networkSecurityGroupName) |> Async.AwaitTask

        let allRules = nsg.Body.SecurityRules

        return allRules
               |> Seq.filter(fun r -> r.Name.StartsWith(rulePrefix))
               |> Seq.toList
    }

/// Delete the security rule corresponding to the given security rule name from NSG
let deleteSecurityRule (context:InfrastructureContext) groupName networkSecurityGroupName securityRuleName =
    async {
        return! context.network.SecurityRules.DeleteWithHttpMessagesAsync(groupName, networkSecurityGroupName, securityRuleName) |> Async.AwaitTask
    }

/// Returns true if network security group with the given name (parameter networkSecurityGroupName) exists, returns false if it doesn't exist
let isExistingNsg (context:InfrastructureContext) groupName networkSecurityGroupName =
    let azureCloudExceptionType = someExceptionOfType<Microsoft.Rest.Azure.CloudException>
    async {
        try
            let! nsgResponse = context.network.NetworkSecurityGroups.GetWithHttpMessagesAsync(groupName, networkSecurityGroupName) |> Async.AwaitTask
            let nsg = nsgResponse.Body
            return not <| isNull nsg
        with
        | IsAggregateOf azureCloudExceptionType ex ->
            TraceTags.warning "Could not retrieve NSG information"
                (context.tags @ [
                    "groupName", groupName
                    "networkSecurityGroupName", networkSecurityGroupName
                    "exception", ex.ToString() ])
            return false
    }

/// Delete the specified network interface
let deleteNetworkInterface (context:InfrastructureContext) groupName networkInterfaceName =
    async {
        return! context.network.NetworkInterfaces.DeleteWithHttpMessagesAsync(groupName, networkInterfaceName) |> Async.AwaitTask
    }