$starWarsApiProjPath = "samples/star-wars-api/star-wars-api.fsproj"
[xml]$starWarsApiProj = Get-Content -Path $starWarsApiProjPath
$starWarsApiProjRefNode = $starWarsApiProj.SelectSingleNode("//ItemGroup[@Label='ProjectReferences']")
$starWarsApiProjRefNode.ParentNode.RemoveChild($starWarsApiProjRefNode) | Out-Null

$dirBuildTargetsPath = "Directory.Build.targets"
[xml]$dirBuildTargets = Get-Content -Path $dirBuildTargetsPath
$version = $dirBuildTargets.SelectSingleNode("//PropertyGroup[@Label='NuGet']/Version").InnerText

[xml]$fsharpPackages = @"
<ItemGroup Label="PackageReferences">
    <PackageReference Include="FSharp.Data.GraphQL.Server.AspNetCore" Version="$($version)" />
    <PackageReference Include="FSharp.Data.GraphQL.Server.Middleware" Version="$($version)" />
    <PackageReference Include="FSharp.Data.GraphQL.Server.Relay" Version="$($version)" />
    <PackageReference Include="FSharp.Data.GraphQL.Server" Version="$($version)" />
    <PackageReference Include="FSharp.Data.GraphQL.Shared" Version="$($version)" />
</ItemGroup>
"@

$packagesPropsPath = "Packages.props"
[xml]$packagesProps = Get-Content -Path $packagesPropsPath
$packageReferences = $starWarsApiProj.SelectSingleNode("//ItemGroup[@Label='PackageReferences']")
foreach($packageReference in $fsharpPackages.DocumentElement.ChildNodes){
    $innerNode = $starWarsApiProj.ImportNode($packageReference,$true)
    $packageReferences.AppendChild($innerNode)
}
$starWarsApiProj.Save($starWarsApiProjPath)