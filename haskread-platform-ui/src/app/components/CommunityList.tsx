import React from 'react';
import { useRouter } from 'next/navigation'

interface CommunityListProps {
    communities: Array<{
        communityID: number;
        communityName: string;
    }>;
    setCommunityId: (communityId: number) => void;
}

const CommunityList: React.FC<CommunityListProps> = ({ communities,setCommunityId }) => {
    const router = useRouter();
    const goToCommunityId = (communityId : number) => {
        setCommunityId(communityId)
        router.push(`/?community_id=${communityId}`)
    }

    return (
            <div className='card-bg shadow-lg rounded-lg mb-6 overflow-hidden'>
            <div className='border-b p-4'>
                <p className="text-lg font-bold text-gray-800">Communities</p>
            </div>
            <div className='p-4'>
            <ul className='space-y-2'>
                {communities.map((community,index) => (
                <div key={index}>
                    <li key={community.communityID} className="mb-2">
                        <button
                            onClick={() => goToCommunityId(community.communityID)}
                            className="block text-blue-600 hover:underline"
                        >
                            {community.communityName}
                        </button>
                    </li>
                    <hr className='border-gray-300' /> 
                    </div>
                ))}
            </ul>
            </div>
            </div>
    );
}

export default CommunityList;
